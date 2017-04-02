package dsscratch.algos.broadcast

import dsscratch.algos.AlgoCode
import dsscratch.algos.AlgoCodes
import dsscratch.clocks.TimeStamp
import dsscratch.components.Message
import dsscratch.components.Process
import dsscratch.components._


import scala.collection.mutable.{Set => mSet, Map => mMap}

/*
When a Broadcast command is received at a node, it sends an Echo message to its
neighbors, kicking off the echo broadcast algorithm.

When a node receives an Echo command, if it has no echo parent, it records the
sender as its parent and delivers the contained message to itself. It then
sends the message over all its channels except to its parent. It only sends to
its parent once it has received the same Echo over all incoming channels.
*/

trait EchoLocalState extends LocalState {
  var initiated: mSet[Command]
  var echoParentIds: mMap[Command, ProcessId]
  var toReceive: mMap[Command, mSet[ProcessId]]
}

object EchoModule extends NodeModuleBuilder {
  def apply(parentNode: Node, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): EchoModule = {
    new EchoModule(parentNode)
  }
  def buildWith(parentNode: Node, s: EchoLocalState): EchoModule = {
    val newC = new EchoModule(parentNode)

    //Set state properties
    newC.s.initiated = s.initiated
    newC.s.echoParentIds = s.echoParentIds
    newC.s.toReceive = s.toReceive
    newC
  }
}

class EchoModule(val parentNode: Node) extends NodeModule {
  val algoCode: AlgoCode = AlgoCodes.ECHO

  ////////////////////
  //LOCAL STATE
  private object s extends EchoLocalState {
    var initiated: mSet[Command] = mSet[Command]()
    var echoParentIds: mMap[Command, ProcessId] = mMap[Command, ProcessId]()
    var toReceive: mMap[Command, mSet[ProcessId]] =
      mMap[Command, mSet[ProcessId]]()
  }
  ////////////////////

  def processMessage(cmd: Command, chSenderId: ProcessId, ts: TimeStamp):
    Unit = {
    cmd match {
      case Broadcast(cmd, senderId, ts) => initiateEcho(cmd, senderId, ts)
      case e @ Echo(cmd, _, _) =>
        if (!terminatedFor(cmd)) processEcho(cmd, chSenderId)
      case _ => // do nothing
    }
  }

  def terminated: Boolean = {
    if (s.initiated.isEmpty) return false

    (for ((cmd, _) <- s.toReceive) yield {
      initiatedFor(cmd) && terminatedFor(cmd);
    }).forall(b => b)
  }

  private def initiatedFor(cmd: Command): Boolean = s.initiated.contains(cmd)

  private def terminatedFor(cmd: Command): Boolean =
    isComplete(s.toReceive.get(cmd))

  def step(): Unit = {
    if (terminated) return
    // nothing to do...
  }

  def snapshot: EchoModule = EchoModule.buildWith(parentNode, s)

  def result = ""

  private def initiateEcho(cmd: Command, senderId: ProcessId, ts: TimeStamp): Unit =
  {
    if (!isInitiatorFor(cmd)) {
      val newEcho = Echo(cmd, senderId, ts)
      for (targetId <- outProcessIds) {
        val msg = Message(newEcho, parentNode.id, clock.stamp())
        parentNode.send(targetId, msg)
      }
      setEchoParentFor(cmd, parentNode.id)
      s.initiated += cmd
    }
  }

  private def processEcho(cmd: Command, senderId: ProcessId): Unit = {
    // If we initiated this echo, ignore incoming echo messages
    if (isInitiatorFor(cmd)) return

    if (!hasInitiatedChsFor(cmd)) generateInChsFor(cmd)
    removeSenderFromReceiveSet(cmd, senderId)

    val newEcho = Echo(cmd, parentNode.id, clock.stamp())
    val newMsg = Message(newEcho, parentNode.id, clock.stamp())

    if (!hasProcessed(cmd)) {
      s.initiated += cmd
      setEchoParentFor(cmd, senderId)
      for (targetId <- outProcessIds.filter(_ != parentNode.id))
        parentNode.send(targetId, newMsg)
      // Deliver wrapped command to self
      val deliverable = Message(cmd, senderId, clock.stamp())
      parentNode.deliver(deliverable)
    }

    if (hasReceivedAllMsgsFor(cmd)) {
      val echoParentId = s.echoParentIds(cmd)
      parentNode.send(echoParentId, newMsg)
    }
  }

  private def generateInChsFor(cmd: Command): Unit = {
    val newInNodeIds = mSet[ProcessId]()
    for (newId <- inProcessIds) newInNodeIds += newId
    s.toReceive(cmd) = newInNodeIds
  }

  private def removeSenderFromReceiveSet(cmd: Command, senderId: ProcessId):
    Unit = {
    s.toReceive(cmd) -= senderId
  }

  private def hasProcessed(cmd: Command): Boolean =
    s.echoParentIds.get(cmd).nonEmpty

  private def hasInitiatedChsFor(cmd: Command): Boolean =
    s.toReceive.get(cmd).nonEmpty

  private def hasReceivedAllMsgsFor(cmd: Command): Boolean =
    isComplete(s.toReceive.get(cmd))

  private def setEchoParentFor(cmd: Command, id: ProcessId): Unit =
    s.echoParentIds(cmd) = id

  private def isInitiatorFor(cmd: Command): Boolean =
    s.echoParentIds.get(cmd) match {
      case Some(echoParentId) => parentNode.id == echoParentId
      case _ => false
    }

  private def isComplete[A](o: Option[mSet[ProcessId]]): Boolean = {
    o match {
      case Some(set) => set.isEmpty
      case _ => false
    }
  }
}

