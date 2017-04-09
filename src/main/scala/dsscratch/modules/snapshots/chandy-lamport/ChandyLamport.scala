package dsscratch.modules.tarry

import dsscratch.components._
import dsscratch.modules._
import dsscratch.modules.snapshots._
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
  var nodeIdStates: mMap[ProcessId, ArrayBuffer[Command]]
}

object ChandyLamportModule extends ModuleBuilder {
  def apply(parent: ModuleParent, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): ChandyLamportModule = {
    new ChandyLamportModule(parent, isInitiator)
  }
  def buildWith(parent: ModuleParent, s: ChandyLamportLocalState):
    ChandyLamportModule = {
    val newC = new ChandyLamportModule(parent, s.initiator)

    newC.s.initiated = s.initiated
    newC.s.nodeIdsSent = s.nodeIdsSent.map(x => x)
    newC.s.nodeIdsReceived = s.nodeIdsReceived.map(x => x)
    newC.s.controlMessages = s.controlMessages.map(x => x)
    newC.s.nodeIdStates = s.nodeIdStates.map(x => x)
    newC
  }
}

class ChandyLamportModule(val parent: ModuleParent, isInitiator: Boolean = false)
  extends Module {
  val moduleCode: ModuleCode = ModuleCodes.CHANDY_LAMPORT

  ////////////////////
  //LOCAL STATE
  private object s extends ChandyLamportLocalState {
    var initiated = false
    var initiator: Boolean = isInitiator
    var nodeIdsSent = ArrayBuffer[ProcessId]()
    var nodeIdsReceived = ArrayBuffer[ProcessId]()
    var controlMessages = ArrayBuffer[Command]()
    var nodeIdStates = {
      val nis = mMap[ProcessId, ArrayBuffer[Command]]()
      inProcessIds.foreach((id: ProcessId) =>
        nis.update(id, ArrayBuffer[Command]()))
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
        s.nodeIdStates(newNodeId).append(cmd)
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

  def snapshot: ChandyLamportModule = ChandyLamportModule.buildWith(parent,
    s)

  def result = {
    val status = if (terminated) "Terminated" else "Not Terminated"
    parent + ": " + status
  }

  private def initiate(): Unit = {
    val cmd = TakeSnapshot(parent.clock.stamp())
    s.controlMessages.append(cmd)
    val firstNodeId = Rand.pickItem(outProcessIds)
    log.write("[CL] Sending to Node" + firstNodeId, clock.stamp())
    parent.send(firstNodeId, cmd)
    s.nodeIdsSent.append(firstNodeId)
    parent.takeSnapshot(cmd.snapId)
    s.initiated = true
  }

  private def takeSnapshot(cmd: TakeSnapshot, senderId: ProcessId): Unit = {
    if (s.nodeIdsReceived.exists(_ == senderId)) return

    val newNodeId = inProcessIds.filter(_ == senderId).head
    s.nodeIdsReceived.append(newNodeId)

    if (!s.initiated) {
      s.controlMessages.append(cmd)
      parent.takeSnapshot(cmd.snapId)
      s.initiated = true
    }
  }

  private def sendNextControlMessage() = {
    val nodeIdsLeft = outProcessIds.filter(!s.nodeIdsSent.contains(_))
    val shuffled: ArrayBuffer[ProcessId] = Random.shuffle(nodeIdsLeft)
    val nextNodeId = shuffled.head
    val cmd = s.controlMessages.last
    log.write("[CL] Sending to Node" + nextNodeId, clock.stamp())
    parent.send(nextNodeId, cmd)
  }
}
