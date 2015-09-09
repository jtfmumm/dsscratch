package dsscratch.components

import dsscratch.clocks.TimeStamp

trait Command


case class Token(id: Int)


case class EmptyCommand() extends Command

case class ProcessToken(t: Token) extends Command

case class TakeSnapshot(snapId: TimeStamp) extends Command

////CRUD
case class Read(key: String) extends Command
case class ReadReply(v: Int) extends Command

case class Update(key: String, v: Int) extends Command

case class Delete(key: String) extends Command

////Broadcast

//General broadcast command
case class Broadcast(cmd: Command, sender: Process, ts: TimeStamp) extends Command

case class SimpleBroadcast(cmd: Command, sender: Process, ts: TimeStamp) extends Command

case class InitiateEcho(cmd: Command, sender: Process, ts: TimeStamp) extends Command
case class Echo(cmd: Command, sender: Process, ts: TimeStamp) extends Command

////Consensus

//General commit command
case class Commit(cmd: Command, sender: Process, ts: TimeStamp) extends Command


//2PC
case class InitiateTwoPC(cmd: Command, sender: Process, ts: TimeStamp) extends Command
case class TwoPCVoteRequest(cmd: Command, sender: Process, ts: TimeStamp) extends Command

trait TwoPCVote
case object TwoPCVoteCommit extends TwoPCVote
case object TwoPCVoteAbort extends TwoPCVote

case class TwoPCVoteReply(v: TwoPCVote, cmd: Command, p: Process) extends Command
case class TwoPCCommit(cmd: Command, sender: Process, ts: TimeStamp) extends Command
case class TwoPCAbort(cmd: Command, sender: Process, ts: TimeStamp) extends Command


//Testing
case object PassTest extends Command
case object FailTest extends Command