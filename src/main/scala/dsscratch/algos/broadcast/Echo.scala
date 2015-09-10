package dsscratch.algos.broadcast

import dsscratch.algos.AlgoCode
import dsscratch.algos.AlgoCodes
import dsscratch.algos.nodes.LocalState
import dsscratch.algos.nodes.NodeComponent
import dsscratch.clocks.TimeStamp
import dsscratch.components.Message
import dsscratch.components.Process
import dsscratch.components._


import scala.collection.mutable.{Set => mSet, Map => mMap}

// When a initiateEcho command is received, a node
// sends a message to its neighbors.

// When a node receives an Echo command, if it has no
// parent, it records the sender as its parent and delivers
// the contained message to itself. It then
// sends the message over all its channels except to its
// parent. It only sends to its parent once its received
// the same Echo over all incoming channels.

trait EchoLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
  var echoParents: mMap[Command, Process]
  var chsToReceive: mMap[Command, mSet[Channel]]
}

object EchoComponent {
  def apply(parentProcess: Process, isInitiator: Boolean = false): EchoComponent = {
    new EchoComponent(parentProcess, isInitiator)
  }
  def buildWith(parentProcess: Process, s: EchoLocalState): EchoComponent = {
    val newC = EchoComponent(parentProcess, s.initiator)

    //Set state properties
    newC.s.initiated = s.initiated
    newC.s.echoParents = s.echoParents
    newC.s.chsToReceive = s.chsToReceive
    newC
  }
}

class EchoComponent(val parentProcess: Process, isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.ECHO
  val outChs = parentProcess.outChs
  val inChs = parentProcess.inChs

  ////////////////////
  //LOCAL STATE
  private object s extends EchoLocalState {
    var initiated = false
    var initiator: Boolean = isInitiator
    var echoParents: mMap[Command, Process] = mMap[Command, Process]()
    var chsToReceive: mMap[Command, mSet[Channel]] = mMap[Command, mSet[Channel]]()//mSet[Channel](inChs: _*)
  }
  ////////////////////

  def processMessage(m: Message): Unit = {
    m.cmd match {
      case Broadcast(cmd, proc, ts) => {
        val initiateEcho = InitiateEcho(cmd, proc, ts)
        parentProcess.recv(Message(initiateEcho, parentProcess, clock.stamp()))
      }
      case i @ InitiateEcho(cmd, proc, ts) => initiateEcho(cmd, proc, ts)
      case e @ Echo(cmd, _, _) => if (!terminatedFor(cmd)) processEcho(cmd, m.sender)
      case _ => // do nothing
    }
  }

  def terminated: Boolean = false

  def terminatedFor(cmd: Command): Boolean = s.chsToReceive.get(cmd) match {
    case Some(chs) => chs.isEmpty
    case None => false
  }

  def step(): Unit = {
    if (parentProcess.failed) return
    if (terminated) return
    // nothing to do...
  }

  def snapshot: EchoComponent = EchoComponent.buildWith(parentProcess, s)

  def result = ""

  private def initiateEcho(cmd: Command, proc: Process, ts: TimeStamp): Unit = {
    println("Initiating Echo at " + parentProcess + " for " + cmd)
      val newEcho = Echo(cmd, proc, ts)
      println(outChs)
      for (c <- outChs) c.recv(Message(newEcho, parentProcess, clock.stamp()))
      setParentFor(cmd, parentProcess)
  }

  private def processEcho(cmd: Command, sender: Process) = {
    println("Processing Echo at " + parentProcess + " for " + cmd)
    if (!hasInitiatedChsFor(cmd)) generateInChsFor(cmd)

    val newEcho = Echo(cmd, parentProcess, clock.stamp())
    val newMsg = Message(newEcho, parentProcess, clock.stamp())
    val deliverable = Message(cmd, sender, clock.stamp())

    if (sender != parentProcess) {
      s.chsToReceive(cmd) -= inChs.filter(_.hasSource(sender)).head
    }

    if (!hasProcessed(cmd)) {
      setParentFor(cmd, sender)
      for (c <- outChs.filter(!_.hasTarget(sender))) c.recv(newMsg)
      // Deliver command to self
      parentProcess.recv(deliverable)
    } else if (hasReceivedAllMsgsFor(cmd)) {
      val echoParent = s.echoParents(cmd)
      if (echoParent != parentProcess) outChs.filter(_.hasTarget(echoParent)).head.recv(newMsg)
    }
  }

  private def generateInChsFor(cmd: Command): Unit = {
    val newInChs = mSet[Channel]()
    for (c <- inChs) newInChs += c
    s.chsToReceive(cmd) = newInChs
  }

  private def hasProcessed(cmd: Command): Boolean = s.echoParents.get(cmd).nonEmpty

  private def hasInitiatedChsFor(cmd: Command): Boolean = s.chsToReceive.get(cmd).nonEmpty

  private def hasReceivedAllMsgsFor(cmd: Command): Boolean = s.chsToReceive.get(cmd).isEmpty

  private def setParentFor(cmd: Command, p: Process): Unit = s.echoParents(cmd) = p
}

