package dsscratch.algos.test

import dsscratch.algos.AlgoCode
import dsscratch.algos.AlgoCodes
import dsscratch.algos.nodes.LocalState
import dsscratch.algos.nodes.NodeComponent
import dsscratch.components._

import scala.collection.mutable.ArrayBuffer

// Initiator sends out an InitiateEcho command and then does nothing.
// Non-initiators do nothing.

trait SimpleBroadcastTesterLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
}

object SimpleBroadcastTesterComponent {
  def apply(parentProcess: Process, isInitiator: Boolean = false): SimpleBroadcastTesterComponent = {
    new SimpleBroadcastTesterComponent(parentProcess, isInitiator)
  }
  def buildWith(parentProcess: Process, s: SimpleBroadcastTesterLocalState): SimpleBroadcastTesterComponent = {
    val newC = SimpleBroadcastTesterComponent(parentProcess, s.initiator)

    newC.s.initiated = s.initiated
    newC
  }
}

class SimpleBroadcastTesterComponent(val parentProcess: Process, isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.ECHO_TESTER
  val outChs: ArrayBuffer[Channel] = parentProcess.outChs
  val inChs: ArrayBuffer[Channel] = parentProcess.inChs

  ////////////////////
  //LOCAL STATE
  private object s extends SimpleBroadcastTesterLocalState {
    var initiated = false
    var initiator: Boolean = isInitiator
  }
  ////////////////////

  def processMessage(m: Message): Unit = {}

  def terminated: Boolean = !s.initiator || s.initiated

  def step(): Unit = {
    if (parentProcess.failed) return
    if (s.initiator && !s.initiated) initiate()
  }

  def snapshot: SimpleBroadcastTesterComponent = SimpleBroadcastTesterComponent.buildWith(parentProcess, s)

  def result = ""

  private def initiate(): Unit = {
    val newInitiate = Broadcast(PassTest, parentProcess, clock.stamp())
    val broadcast = Message(newInitiate, parentProcess, clock.stamp())
    parentProcess.recv(broadcast)
    s.initiated = true
  }
}
