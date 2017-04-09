package fitzroy.modules.consensus

import fitzroy.clocks.TimeStamp
import fitzroy.components._

////////////
// Commands
////////////
//General commit command
case class Commit(cmd: Command, senderId: ProcessId, ts: TimeStamp)
  extends Command

//Two Phase Commit
case class InitiateTwoPC(cmd: Command, senderId: ProcessId, ts: TimeStamp)
  extends Command
case class TwoPCVoteRequest(cmd: Command, senderId: ProcessId, ts: TimeStamp)
  extends Command

trait TwoPCVote
case object TwoPCVoteCommit extends TwoPCVote
case object TwoPCVoteAbort extends TwoPCVote

case class TwoPCVoteReply(v: TwoPCVote, cmd: Command, pId: ProcessId)
  extends Command
case class TwoPCCommit(cmd: Command, senderId: ProcessId, ts: TimeStamp)
  extends Command
case class TwoPCAbort(cmd: Command, senderId: ProcessId, ts: TimeStamp)
  extends Command
