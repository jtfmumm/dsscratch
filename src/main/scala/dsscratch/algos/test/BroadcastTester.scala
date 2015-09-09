package dsscratch.algos.test

import dsscratch.algos.AlgoCode
import dsscratch.algos.AlgoCodes
import dsscratch.algos.nodes.LocalState
import dsscratch.algos.nodes.NodeComponent
import dsscratch.components._

import scala.collection.mutable.ArrayBuffer

// Initiator sends out a Broadcast command. This kicks
// off whatever broadcast protocol is implemented on the nodes.

// All nodes act as a store for test messages.
// Protocol checks that a message is delivered once and only once.

// To use it, another
// algorithm (the one being tested) should attempt to broadcast
// a PassTest command. You can inspect the result at each
// node to see if every node passed.

trait BroadcastTesterLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
  var testResult: (TestCode, Process)
}

object BroadcastTesterComponent {
  def apply(parentProcess: Process, isInitiator: Boolean = false): BroadcastTesterComponent = {
    new BroadcastTesterComponent(parentProcess, isInitiator)
  }
  def buildWith(parentProcess: Process, s: BroadcastTesterLocalState): BroadcastTesterComponent = {
    val newC = BroadcastTesterComponent(parentProcess, s.initiator)

    newC.s.initiated = s.initiated
    newC.s.testResult = s.testResult
    newC
  }
}

class BroadcastTesterComponent(val parentProcess: Process, isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.BROADCAST_TESTER
  val outChs: ArrayBuffer[Channel] = parentProcess.outChs
  val inChs: ArrayBuffer[Channel] = parentProcess.inChs

  ////////////////////
  //LOCAL STATE
  private object s extends BroadcastTesterLocalState {
    var initiated = false
    var initiator: Boolean = isInitiator
    var testResult: (TestCode, Process) = (TestCodes.NO_VAL, EmptyProcess)
  }
  ////////////////////

  def processMessage(m: Message): Unit = {
    m.cmd match {
      case PassTest => s.testResult match {
        case (TestCodes.NO_VAL, _) => s.testResult = (TestCodes.SUCCESS, m.sender)
        case (TestCodes.SUCCESS, _) => s.testResult = (TestCodes.FAILURE, m.sender)
        case (TestCodes.FAILURE, _) => //Do nothing
      }
      case FailTest => s.testResult = (TestCodes.FAILURE, m.sender)
      case _ => // do nothing...
    }
  }

  // Use the algorithm being tested to determine if the test has terminated.
  def terminated: Boolean = false

  def step(): Unit = {
    if (parentProcess.failed) return
    if (s.initiator && !s.initiated) initiate()
  }

  def snapshot: BroadcastTesterComponent = BroadcastTesterComponent.buildWith(parentProcess, s)

  def result = parentProcess + ": " + s.testResult._1 + " received from " + s.testResult._2 + "\n"

  private def initiate(): Unit = {
    val newBroadcast = Broadcast(PassTest, parentProcess, clock.stamp())
    val echo = Message(newBroadcast, parentProcess, clock.stamp())
    parentProcess.recv(echo)
    s.initiated = true
  }
}
