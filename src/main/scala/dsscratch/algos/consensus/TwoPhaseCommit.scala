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
  var curVote: Option[Message]
  var initiatedMsg: Option[Message]
  var votes: mMap[Message, mMap[Process, TwoPCVote]]
}

object TwoPhaseCommitComponent {
  def apply(parentProcess: Process, isInitiator: Boolean = false): TwoPhaseCommitComponent = {
    new TwoPhaseCommitComponent(parentProcess, isInitiator)
  }
  def buildWith(parentProcess: Process, s: TwoPhaseCommitLocalState): TwoPhaseCommitComponent = {
    val newC = TwoPhaseCommitComponent(parentProcess, s.initiator)

    newC.s.initiated = s.initiated
    newC.s.curVote = None
    newC.s.initiatedMsg = None
    newC
  }
}

class TwoPhaseCommitComponent(val parentProcess: Process, nodes: Seq[Process], isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.NONE // <-- use code
  val outChs: ArrayBuffer[Channel] = parentProcess.outChs
  val inChs: ArrayBuffer[Channel] = parentProcess.inChs

  ////////////////////
  //LOCAL STATE
  private object s extends TwoPhaseCommitLocalState {
    var initiated = false
    var initiator: Boolean = isInitiator
    var curVote: Option[Message] = None
    var initiatedMsg: Option[Message] = None
  }
  ////////////////////

  def processMessage(m: Message): Unit = {
    m.cmd match {
      case InitiateTwoPC(msg) => if (m.sender == parentProcess) initiate2PC(msg)
      case TwoPCVoteRequest(msg) =>
      case r @ TwoPCVoteReply(vote, msg, process) => if (!isInitiatorFor(msg)) registerReply(r)
      case TwoPCVoteCommit(msg) => if (!isInitiatorFor(msg)) commit(msg)
      case TwoPCVoteAbort(msg) => if (!isInitiatorFor(msg)) abort(msg)
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

  private def initiate2PC(m: Message): Unit = {
    val voteRequest = Message(TwoPCVoteRequest(m), parentProcess, m.ts)
    val initiateEchoMsg = Message(InitiateEcho(voteRequest), parentProcess, m.ts)
    s.initiatedMsg = Some(m)
    s.votes.update(m, mMap[Process, TwoPCVote]())
    parentProcess.recv(initiateEchoMsg)
  }

  private def registerReply(r: TwoPCVoteReply) = r match {
    case TwoPCVoteReply(vote, msg, p) => {
      s.votes(msg).update(p, vote)
      if (allVotesReceived(msg)) checkForSuccessfulVoteFor(msg)
    }
  }

  private def allVotesReceived(m: Message): Boolean = {
    nodes.forall(p => {
      (for (k <- s.votes(m).keys) yield s.votes(m).contains(p)).forall(x => x)
    })
  }

  private def checkForSuccessfulVoteFor(m: Message): Unit = {
    val result = if (voteSucceedsFor(m)) TwoPCCommit(m) else TwoPCAbort(m)
    val echoMsg = Message(result, parentProcess, m.ts)
    val initiateEchoMsg = Message(InitiateEcho(echoMsg), parentProcess, m.ts)
    parentProcess.recv(initiateEchoMsg)
  }

  private def commit(m: Message): Unit = {
    if (s.curVote.nonEmpty) {
      parentProcess.recv(m)
      s.curVote = None
    }
  }

  private def abort(m: Message): Unit = {
    s.curVote = None
  }

  private def voteSucceedsFor(m: Message): Boolean = {
    (for (k <- s.votes(m).keys) yield s.votes(m)(k) == TwoPCVoteCommit(m)).forall(x => x)
  }

  private def isInitiatorFor(m: Message): Boolean = {
    s.initiatedMsg == Some(m)
  }

  private def initiate(): Unit = {
    //Do some initiation work...
    s.initiated = true
  }
}

