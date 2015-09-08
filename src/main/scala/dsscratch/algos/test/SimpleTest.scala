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

// This algorithm checks that a message is delivered once
// and only once.
//
// It doesn't actually do anything. To use it, another
// algorithm (the one being tested) should attempt to broadcast
// a PassTest command. You can inspect the result at each
// node to see if every node passed.


trait SimpleTestLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
  var testResult: (TestCode, Process)
}

object SimpleTestComponent {
  def apply(parentProcess: Process, isInitiator: Boolean = false): SimpleTestComponent = {
    new SimpleTestComponent(parentProcess, isInitiator)
  }
  def buildWith(parentProcess: Process, s: SimpleTestLocalState): SimpleTestComponent = {
    val newC = SimpleTestComponent(parentProcess, s.initiator)

    newC.s.initiated = s.initiated
    newC.s.testResult = s.testResult
    newC
  }
}

class SimpleTestComponent(val parentProcess: Process, isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.NONE // <-- use code
  val outChs: ArrayBuffer[Channel] = parentProcess.outChs
  val inChs: ArrayBuffer[Channel] = parentProcess.inChs

  ////////////////////
  //LOCAL STATE
  private object s extends SimpleTestLocalState {
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
      case _ => // Ignore the rest
    }
  }

  // Use the algorithm being tested to determine if the test has terminated.
  def terminated: Boolean = false //set termination condition

  def step(): Unit = {}

  def snapshot: SimpleTestComponent = SimpleTestComponent.buildWith(parentProcess, s)

  def result = parentProcess + ": " + s.testResult._1 + " from " + s.testResult._2
}

