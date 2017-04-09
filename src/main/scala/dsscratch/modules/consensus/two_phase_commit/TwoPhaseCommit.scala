package dsscratch.modules.consensus.two_phase_commit

import dsscratch.components._
import dsscratch.clocks._
import dsscratch.modules._
import dsscratch.modules.broadcast._
import dsscratch.modules.consensus._

import scala.collection.mutable.{Map => mMap, ArrayBuffer, Set => mSet}

/*
STILL WORK IN PROGRESS
Broadcast protocols
currently only process one message in total
but this protocol requires broadcasting multiple messages

An initiator sends a message to all other nodes
asking for a vote (either yes or abort).

If even one vote is abort, initiator sends out
abort message.

If vote is unanimously yes, initiator sends out
commit message.
*/

trait TwoPhaseCommitLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
  var curVote: Option[Command]
  var initiatedCmds: mSet[Command]
  var votes: mMap[Command, mMap[ProcessId, TwoPCVote]]
  var votedOn: mSet[Command]
  var committed: mSet[Command]
  var networkNodeIds: Set[ProcessId]
}

object TwoPhaseCommitModule extends ModuleBuilder {
  def apply(parent: ModuleParent, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): TwoPhaseCommitModule = {
    new TwoPhaseCommitModule(parent, nodeIds, isInitiator)
  }
  def buildWith(parent: ModuleParent, s: TwoPhaseCommitLocalState): TwoPhaseCommitModule = {
    val newC = TwoPhaseCommitModule(parent, s.networkNodeIds, s.initiator)

    newC.s.initiated = s.initiated
    newC.s.curVote = s.curVote
    newC.s.initiatedCmds = s.initiatedCmds
    newC.s.votedOn = s.votedOn
    newC.s.committed = s.committed
    newC
  }
}

class TwoPhaseCommitModule(val parent: ModuleParent, nodeIds: Set[ProcessId],
  isInitiator: Boolean = false) extends Module {
  val moduleCode: ModuleCode = ModuleCodes.TWO_PHASE_COMMIT

  ////////////////////
  //LOCAL STATE
  private object s extends TwoPhaseCommitLocalState {
    var initiated = false
    var initiator: Boolean = isInitiator
    var curVote: Option[Command] = None
    var initiatedCmds: mSet[Command] = mSet[Command]()
    var votes = mMap[Command, mMap[ProcessId, TwoPCVote]]()
    var votedOn = mSet[Command]()
    var committed = mSet[Command]()
    var networkNodeIds = nodeIds
  }
  ////////////////////

  def processMessage(cmd: Command, chSenderId: ProcessId, ts: TimeStamp): Unit = {
    cmd match {
      case Commit(cmd, senderId, ts) => {
        val initiateTwoPC = InitiateTwoPC(cmd, senderId, ts)
        parent.deliver(initiateTwoPC)
      }
      case InitiateTwoPC(cmd, _, _)  =>
        if (chSenderId == parent.id) initiate2PC(cmd)
      case TwoPCVoteRequest(cmd, _, _) => {
        if (isInitiatorFor(cmd)) return
        if (!s.votedOn.contains(cmd)) sendVote(cmd)
      }
      case r @ TwoPCVoteReply(vote, cmd, process) =>
        if (isInitiatorFor(cmd)) registerReply(r)
      case TwoPCCommit(cmd, _, _) => commit(cmd)
      case TwoPCAbort(cmd, _, _) => abort(cmd)
      case _ => // Ignore the rest
    }
  }

  //checks for one successful voting round, whether or not it was aborted
  def terminated: Boolean = s.committed.nonEmpty

  def step(): Unit = {
    if (terminated) return
    // Nothing to do...
  }

  def snapshot: TwoPhaseCommitModule =
    TwoPhaseCommitModule.buildWith(parent, s)

  def result = "" // For printing results

  private def initiate2PC(cmd: Command): Unit = {
    val voteRequest = TwoPCVoteRequest(cmd, parent.id, clock.stamp())
    val initiateBroadcast = Broadcast(voteRequest, parent.id, clock.stamp())
    s.initiatedCmds += cmd
    s.votedOn += cmd
    s.votes.update(cmd, mMap[ProcessId, TwoPCVote]())
    parent.deliver(initiateBroadcast)
  }

  private def registerReply(r: TwoPCVoteReply) = r match {
    case TwoPCVoteReply(vote, cmd, nodeId) => {
      s.votes(cmd).update(nodeId, vote)
      if (allVotesReceived(cmd)) checkForSuccessfulVoteFor(cmd)
    }
  }

  private def allVotesReceived(cmd: Command): Boolean = {
    s.networkNodeIds.filter(_ != parent.id).forall(nodeId => {
      s.votes(cmd).contains(nodeId)
    })
  }

  private def checkForSuccessfulVoteFor(cmd: Command): Unit = {
    val success = voteSucceedsFor(cmd)
    val result =
      if (success)
        TwoPCCommit(cmd, parent.id, clock.stamp())
      else TwoPCAbort(cmd, parent.id, clock.stamp())
        val initiateBroadcast = Broadcast(result, parent.id, clock.stamp())
    parent.deliver(initiateBroadcast)
    s.committed += cmd
    if (success) parent.deliver(cmd)
  }

  private def sendVote(cmd: Command): Unit = {
    val vote = if (s.curVote.isEmpty) TwoPCVoteCommit else TwoPCVoteAbort
    val reply = TwoPCVoteReply(vote, cmd, parent.id)
    val initiateBroadcast = Broadcast(reply, parent.id, clock.stamp())
    parent.deliver(initiateBroadcast)
    s.votedOn += cmd
  }

  private def commit(cmd: Command): Unit = {
    parent.deliver(cmd)
    s.committed += cmd
    s.curVote = None
  }

  private def abort(cmd: Command): Unit = {
    s.curVote = None
  }

  private def voteSucceedsFor(cmd: Command): Boolean = {
    (for (k <- s.votes(cmd).keys) yield
      isCommitVote(s.votes(cmd)(k))).forall(x => x)
  }

  private def isCommitVote(vote: TwoPCVote): Boolean = vote match {
    case TwoPCVoteCommit => true
    case TwoPCVoteAbort => false
  }

  private def isInitiatorFor(cmd: Command): Boolean =
    s.initiatedCmds.contains(cmd)
}

