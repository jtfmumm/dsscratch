package dsscratch.components

import dsscratch.clocks.TimeStamp

trait Command


case class Token(id: Int)


case class EmptyCommand() extends Command

case class ProcessToken(t: Token) extends Command

case class TakeSnapshot(snapId: TimeStamp) extends Command

//CRUD
case class Read(key: String) extends Command
case class ReadReply(v: Int) extends Command

case class Update(key: String, v: Int) extends Command

case class Delete(key: String) extends Command

//Broadcast
case class Broadcast(cmd: Command) extends Command

case class InitiateEcho(cmd: Command) extends Command
case class Echo(cmd: Command) extends Command

//Consensus
//2PC
case class InitiateTwoPC(cmd: Command) extends Command
case class TwoPCVoteRequest(cmd: Command) extends Command

trait TwoPCVote
case class TwoPCVoteCommit(cmd: Command) extends TwoPCVote
case class TwoPCVoteAbort(cmd: Command) extends TwoPCVote

case class TwoPCVoteReply(v: TwoPCVote, cmd: Command, p: Process) extends Command
case class TwoPCCommit(cmd: Command) extends Command
case class TwoPCAbort(cmd: Command) extends Command


//Testing
case object PassTest extends Command
case object FailTest extends Command