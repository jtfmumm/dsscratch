package dsscratch.algos.test

import dsscratch.algos.AlgoCode
import dsscratch.algos.AlgoCodes
import dsscratch.clocks._
import dsscratch.components._

import scala.collection.mutable.ArrayBuffer

/*
Initiator sends out a Broadcast command. This kicks off whatever broadcast
protocol is implemented on the nodes.

All nodes act as a store for test messages.
Protocol checks that a message is delivered once and only once.

To use it, another algorithm (the one being tested) should attempt to broadcast
a PassTest command. You can inspect the result at each node to see if every
node passed.
*/

trait BroadcastTesterLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
  var testResult: (TestCode, ProcessId)
}

object BroadcastTesterModule extends NodeModuleBuilder {
  def apply(parentNode: Node, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): BroadcastTesterModule = {
    new BroadcastTesterModule(parentNode, isInitiator)
  }
  def buildWith(parentNode: Node, s: BroadcastTesterLocalState):
    BroadcastTesterModule = {
    val newC = new BroadcastTesterModule(parentNode, s.initiator)

    newC.s.initiated = s.initiated
    newC.s.testResult = s.testResult
    newC
  }
}

class BroadcastTesterModule(val parentNode: Node,
  isInitiator: Boolean = false) extends NodeModule {
  val algoCode: AlgoCode = AlgoCodes.BROADCAST_TESTER

  ////////////////////
  //LOCAL STATE
  private object s extends BroadcastTesterLocalState {
    var initiated = false
    var initiator: Boolean = isInitiator
    var testResult: (TestCode, ProcessId) = (TestCodes.NO_VAL, ProcessId.empty)
  }
  ////////////////////

  def processMessage(cmd: Command, chSenderId: ProcessId, ts: TimeStamp): Unit = {
    cmd match {
      case PassTest => s.testResult match {
        case (TestCodes.NO_VAL, _) =>
          s.testResult = (TestCodes.SUCCESS, chSenderId)
        // We shouldn't receive PassTest twice.
        // If we've already set s.testResult, then we've already received
        // a PassTest command.
        case (_, _) =>
          s.testResult = (TestCodes.FAILURE, chSenderId)
      }
      case FailTest =>
        s.testResult = (TestCodes.FAILURE, chSenderId)
      case _ => // do nothing...
    }
  }

  // Use the algorithm being tested to determine if the test has terminated.
  def terminated: Boolean = false

  def step(): Unit = {
    if (s.initiator && !s.initiated) initiate()
  }

  def snapshot: BroadcastTesterModule =
    BroadcastTesterModule.buildWith(parentNode, s)

  def result =
    s.testResult._2 match {
      case pid if (pid == ProcessId.empty) =>
        parentNode + ": " + s.testResult._1 + "\n"
      case _ => parentNode + ": " + s.testResult._1 + " received from Node" +
        s.testResult._2 + "\n"
    }

  override def testCode = s.testResult._1

  private def initiate(): Unit = {
    val newBroadcast = Broadcast(PassTest, parentNode.id, clock.stamp())
    val msg = Message(newBroadcast, parentNode.id, clock.stamp())
    parentNode.recv(msg)
    s.initiated = true
    // Since we're broadcasting, this node is not waiting to receive PassTest
    s.testResult = (TestCodes.SUCCESS, parentNode.id)
  }
}
