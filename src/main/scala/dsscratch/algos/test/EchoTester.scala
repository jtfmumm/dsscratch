package dsscratch.algos.test

import dsscratch.algos.AlgoCode
import dsscratch.algos.AlgoCodes
import dsscratch.algos.nodes.LocalState
import dsscratch.algos.nodes.NodeComponent
import dsscratch.components.Channel
import dsscratch.components.Message
import dsscratch.components.Process
import dsscratch.components._

import scala.collection.mutable.ArrayBuffer

// Initiator sends out an InitiateEcho command and then does nothing.
// Non-initiators do nothing.

trait EchoTesterLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
}

object EchoTesterComponent {
  def apply(parentProcess: Process, isInitiator: Boolean = false): EchoTesterComponent = {
    new EchoTesterComponent(parentProcess, isInitiator)
  }
  def buildWith(parentProcess: Process, s: EchoTesterLocalState): EchoTesterComponent = {
    val newC = EchoTesterComponent(parentProcess, s.initiator)

    newC.s.initiated = s.initiated
    newC
  }
}

class EchoTesterComponent(val parentProcess: Process, isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.NONE // <-- use code
  val outChs: ArrayBuffer[Channel] = parentProcess.outChs
  val inChs: ArrayBuffer[Channel] = parentProcess.inChs

  ////////////////////
  //LOCAL STATE
  private object s extends EchoTesterLocalState {
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

  def snapshot: EchoTesterComponent = EchoTesterComponent.buildWith(parentProcess, s)

  def result = ""

  private def initiate(): Unit = {
    val passTest = Message(PassTest, parentProcess, clock.stamp())
    val echo = Message(InitiateEcho(passTest), parentProcess, clock.stamp())
    parentProcess.recv(echo)
    s.initiated = true
  }
}
