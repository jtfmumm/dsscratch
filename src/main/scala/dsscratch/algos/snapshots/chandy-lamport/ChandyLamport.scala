package dsscratch.algos.tarry

import dsscratch.components._
import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.clocks.TimeStamp
import dsscratch.util.Rand
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{Map => mMap}
import scala.util.Random

/*
Chandy-Lamport snapshot algorithm
Kicked off by any initiator (here we choose one)
1) The initiator takes a local snapshot and sends control messages through all
channels.
2) When a process receives a control message for the first time, it takes a
local snapshot and sends control messages through all channels.
3) A process p records the state of an incoming channel c by taking a snapshot
of all incoming messages M on c from the time p first received a control
message to the time that a control message is received over c (these can
coincide, in which case M is empty)
4) A process terminates when it has received control messages over all
channels and has sent control messages over all channels.
*/

trait ChandyLamportLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
  var nodeIdsSent: ArrayBuffer[ProcessId]
  var nodeIdsReceived: ArrayBuffer[ProcessId]
  var controlMessages: ArrayBuffer[Command]
  var nodeIdStates: mMap[ProcessId, ArrayBuffer[Message]]
}

object ChandyLamportModule extends NodeModuleBuilder {
  def apply(parentNode: Node, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): ChandyLamportModule = {
    new ChandyLamportModule(parentNode, isInitiator)
  }
  def buildWith(parentNode: Node, s: ChandyLamportLocalState):
    ChandyLamportModule = {
    val newC = new ChandyLamportModule(parentNode, s.initiator)

    newC.s.initiated = s.initiated
    newC.s.nodeIdsSent = s.nodeIdsSent.map(x => x)
    newC.s.nodeIdsReceived = s.nodeIdsReceived.map(x => x)
    newC.s.controlMessages = s.controlMessages.map(x => x)
    newC.s.nodeIdStates = s.nodeIdStates.map(x => x)
    newC
  }
}

class ChandyLamportModule(val parentNode: Node, isInitiator: Boolean = false)
  extends NodeModule {
  val algoCode: AlgoCode = AlgoCodes.CHANDY_LAMPORT

  ////////////////////
  //LOCAL STATE
  private object s extends ChandyLamportLocalState {
    var initiated = false
    var initiator: Boolean = isInitiator
    var nodeIdsSent = ArrayBuffer[ProcessId]()
    var nodeIdsReceived = ArrayBuffer[ProcessId]()
    var controlMessages = ArrayBuffer[Command]()
    var nodeIdStates = {
      val nis = mMap[ProcessId, ArrayBuffer[Message]]()
      inProcessIds.foreach((id: ProcessId) =>
        nis.update(id, ArrayBuffer[Message]()))
      nis
    }
  }
  ////////////////////

  def processMessage(cmd: Command, chSenderId: ProcessId, ts: TimeStamp): Unit = {
    cmd match {
      case c @ TakeSnapshot(_) => takeSnapshot(c, chSenderId)
      case _ => {
        if (s.nodeIdsReceived.exists(_ == chSenderId)) return
        val newNodeId = inProcessIds.filter(_ == chSenderId).head
        // Add this message to the list of messages for our current inCh state
        s.nodeIdStates(newNodeId).append(Message(cmd, chSenderId, ts))
      }
    }
  }

  def terminated: Boolean =
    s.nodeIdsSent.size == outProcessIds.size &&
      s.nodeIdsReceived.size == inProcessIds.size

  def step(): Unit = {
    if (s.initiator && !s.initiated) initiate()
    if (terminated) return
    if (outProcessIds.size > s.nodeIdsSent.size) sendNextControlMessage()
  }

  def snapshot: ChandyLamportModule = ChandyLamportModule.buildWith(parentNode,
    s)

  def result = {
    val status = if (terminated) "Terminated" else "Not Terminated"
    parentNode + ": " + status
  }

  private def initiate(): Unit = {
    val cmd = TakeSnapshot(parentNode.clock.stamp())
    s.controlMessages.append(cmd)
    val firstNodeId = Rand.pickItem(outProcessIds)
    val msg = Message(cmd, parentNode.id, clock.stamp())
    log.write("[CL] Sending to Node" + firstNodeId, msg.ts)
    parentNode.send(firstNodeId, msg)
    s.nodeIdsSent.append(firstNodeId)
    parentNode.takeSnapshot(cmd.snapId)
    s.initiated = true
  }

  private def takeSnapshot(cmd: TakeSnapshot, senderId: ProcessId): Unit = {
    if (s.nodeIdsReceived.exists(_ == senderId)) return

    val newNodeId = inProcessIds.filter(_ == senderId).head
    s.nodeIdsReceived.append(newNodeId)

    if (!s.initiated) {
      s.controlMessages.append(cmd)
      parentNode.takeSnapshot(cmd.snapId)
      s.initiated = true
    }
  }

  private def sendNextControlMessage() = {
    val nodeIdsLeft = outProcessIds.filter(!s.nodeIdsSent.contains(_))
    val shuffled: ArrayBuffer[ProcessId] = Random.shuffle(nodeIdsLeft)
    val nextNodeId = shuffled.head
    val cmd = s.controlMessages.last
    val msg = Message(cmd, parentNode.id, clock.stamp())
    log.write("[CL] Sending to Node" + nextNodeId, msg.ts)
    parentNode.send(nextNodeId, msg)
  }
}
