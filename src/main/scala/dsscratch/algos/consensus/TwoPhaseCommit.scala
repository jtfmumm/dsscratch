package dsscratch.algos.consensus

import dsscratch.components._
import dsscratch.clocks._
import dsscratch.algos._
import dsscratch.algos.nodes._

import scala.collection.mutable.{Map => mMap, ArrayBuffer}

// STILL WORK IN PROGRESS

// An initiator sends a message to all other nodes
// asking for a vote (either yes or abort).
//
// If even one vote is abort, initiator sends out
// abort message.
//
// If vote is unanimously yes, initiator sends out
// commit message.

trait TwoPhaseCommitLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
  var curVote: Option[Command]
  var initiatedCmd: Option[Command]
  var votes: mMap[Command, mMap[Process, TwoPCVote]]
  var networkNodes: Seq[Process]
}

object TwoPhaseCommitComponent {
  def apply(parentProcess: Process, nodes: Seq[Process], isInitiator: Boolean = false): TwoPhaseCommitComponent = {
    new TwoPhaseCommitComponent(parentProcess, nodes, isInitiator)
  }
  def buildWith(parentProcess: Process, s: TwoPhaseCommitLocalState): TwoPhaseCommitComponent = {
    val newC = TwoPhaseCommitComponent(parentProcess, s.networkNodes, s.initiator)

    newC.s.initiated = s.initiated
    newC.s.curVote = None
    newC.s.initiatedCmd = None
    newC
  }
}

class TwoPhaseCommitComponent(val parentProcess: Process, nodes: Seq[Process], isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.TWO_PHASE_COMMIT
  val outChs: ArrayBuffer[Channel] = parentProcess.outChs
  val inChs: ArrayBuffer[Channel] = parentProcess.inChs

  ////////////////////
  //LOCAL STATE
  private object s extends TwoPhaseCommitLocalState {
    var initiated = false
    var initiator: Boolean = isInitiator
    var curVote: Option[Command] = None
    var initiatedCmd: Option[Command] = None
    var votes = mMap[Command, mMap[Process, TwoPCVote]]()
    var networkNodes = nodes
  }
  ////////////////////

  def processMessage(m: Message): Unit = {
    m.cmd match {
      case InitiateTwoPC(cmd, _, _)  => if (m.sender == parentProcess) initiate2PC(cmd)
      case TwoPCVoteRequest(cmd, _, _) => // IMPLEMENT!
      case r @ TwoPCVoteReply(vote, cmd, process) => if (!isInitiatorFor(cmd)) registerReply(r)
      case TwoPCCommit(cmd, _, _) => if (!isInitiatorFor(cmd)) commit(cmd)
      case TwoPCAbort(cmd, _, _) => if (!isInitiatorFor(cmd)) abort(cmd)
      case _ => // Ignore the rest
    }
  }

  def terminated: Boolean = false //set termination condition

  def step(): Unit = {
    if (parentProcess.failed) return
    if (s.initiator && !s.initiated) initiate()
    if (terminated) return
    // Do something each step...
  }

  def snapshot: TwoPhaseCommitComponent = TwoPhaseCommitComponent.buildWith(parentProcess, s)

  def result = "" // For printing results

  private def initiate2PC(cmd: Command): Unit = {
    val voteRequest = TwoPCVoteRequest(cmd, parentProcess, clock.stamp())
    val initiateEchoMsg = Message(InitiateEcho(voteRequest, parentProcess, clock.stamp()), parentProcess, clock.stamp())
    s.initiatedCmd = Some(cmd)
    s.votes.update(cmd, mMap[Process, TwoPCVote]())
    parentProcess.recv(initiateEchoMsg)
  }

  private def registerReply(r: TwoPCVoteReply) = r match {
    case TwoPCVoteReply(vote, cmd, p) => {
      s.votes(cmd).update(p, vote)
      if (allVotesReceived(cmd)) checkForSuccessfulVoteFor(cmd)
    }
  }

  private def allVotesReceived(cmd: Command): Boolean = {
    nodes.forall(p => {
      (for (k <- s.votes(cmd).keys) yield s.votes(cmd).contains(p)).forall(x => x)
    })
  }

  private def checkForSuccessfulVoteFor(cmd: Command): Unit = {
    val success = voteSucceedsFor(cmd)
    val result = if (success) TwoPCCommit(cmd, parentProcess, clock.stamp()) else TwoPCAbort(cmd, parentProcess, clock.stamp())
    val initiateEchoMsg = Message(InitiateEcho(result, parentProcess, clock.stamp()), parentProcess, clock.stamp())
    parentProcess.recv(initiateEchoMsg)
    if (success) parentProcess.recv(Message(cmd, parentProcess, clock.stamp()))
  }

  private def commit(cmd: Command): Unit = {
    if (s.curVote.nonEmpty) {
      val deliverable = Message(cmd, parentProcess, clock.stamp())
      parentProcess.recv(deliverable)
      s.curVote = None
    }
  }

  private def abort(cmd: Command): Unit = {
    s.curVote = None
  }

  private def voteSucceedsFor(cmd: Command): Boolean = {
    (for (k <- s.votes(cmd).keys) yield isCommitVote(s.votes(cmd)(k))).forall(x => x)
  }

  private def isCommitVote(vote: TwoPCVote): Boolean = vote match {
    case TwoPCVoteCommit(_, _, _) => true
    case TwoPCVoteAbort(_, _, _) => false
  }

  private def isInitiatorFor(cmd: Command): Boolean = {
    s.initiatedCmd == Some(cmd)
  }

  private def initiate(): Unit = {
    //Do some initiation work...
    s.initiated = true
  }
}

