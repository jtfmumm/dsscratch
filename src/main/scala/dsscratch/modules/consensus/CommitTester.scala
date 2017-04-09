package dsscratch.modules.consensus

import dsscratch.modules._
import dsscratch.clocks._
import dsscratch.components._
import dsscratch.modules.test._
import dsscratch.topology._

import scala.collection.mutable.ArrayBuffer

/*
Initiator sends out a Commit command. This kicks
off whatever commit/consensus protocol is implemented on the nodes.

All nodes act as a store for test messages.
Protocol checks that a message is delivered once and only once.

To use it, another
algorithm (the one being tested) should attempt to commit
a PassTest command. You can inspect the result at each
node to see if every node passed.
*/

trait CommitTesterLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
  var testResult: (TestCode, ProcessId)
}

object CommitTesterModule extends ModuleBuilder {
  def apply(parent: ModuleParent, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): CommitTesterModule = {
    new CommitTesterModule(parent, isInitiator)
  }
  def buildWith(parent: ModuleParent, s: CommitTesterLocalState):
    CommitTesterModule = {
    val newC = new CommitTesterModule(parent, s.initiator)

    newC.s.initiated = s.initiated
    newC.s.testResult = s.testResult
    newC
  }
}

class CommitTesterModule(val parent: ModuleParent,
  isInitiator: Boolean = false) extends Module {
  val moduleCode: ModuleCode = ModuleCodes.COMMIT_TESTER

  ////////////////////
  //LOCAL STATE
  private object s extends CommitTesterLocalState {
    var initiated = false
    var initiator: Boolean = isInitiator
    var testResult: (TestCode, ProcessId) = (TestCodes.NO_VAL, ProcessId.empty)
  }
  ////////////////////

  def processMessage(cmd: Command, chSenderId: ProcessId, ts: TimeStamp): Unit = {
    cmd match {
      case PassTest => s.testResult match {
        case (TestCodes.NO_VAL, _) => s.testResult = (TestCodes.SUCCESS,
          chSenderId)
        case (TestCodes.SUCCESS, _) => s.testResult = (TestCodes.FAILURE,
          chSenderId)
        case (TestCodes.FAILURE, _) => //Do nothing
        case (_, _) => //Do nothing
      }
      case FailTest => s.testResult = (TestCodes.FAILURE, chSenderId)
      case _ => // do nothing...
    }
  }

  // Use the algorithm being tested to determine if the test has terminated.
  def terminated: Boolean = false

  def step(): Unit = {
    if (s.initiator && !s.initiated) initiate()
  }

  def snapshot: CommitTesterModule =
    CommitTesterModule.buildWith(parent, s)

  def result =
    s.testResult._2 match {
      case pid if (pid == ProcessId.empty) =>
        parent + ": " + s.testResult._1 + "\n"
      case _ => parent + ": " + s.testResult._1 + " received from Node" +
        s.testResult._2 + "\n"
    }

  override def testCode = s.testResult._1

  private def initiate(): Unit = {
    val newCommit = Commit(PassTest, parent.id, clock.stamp())
    parent.deliver(newCommit)
    s.initiated = true
  }
}

