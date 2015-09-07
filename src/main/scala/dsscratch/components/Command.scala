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
case class Broadcast(m: Message) extends Command

case class InitiateEcho(m: Message) extends Command
case class Echo(m: Message) extends Command

//Consensus
//2PC
case class InitiateTwoPC(m: Message) extends Command
case class TwoPCVoteRequest(m: Message) extends Command

trait TwoPCVote
case class TwoPCVoteCommit(m: Message) extends TwoPCVote
case class TwoPCVoteAbort(m: Message) extends TwoPCVote

case class TwoPCVoteReply(v: TwoPCVote, m: Message, p: Process) extends Command
case class TwoPCCommit(m: Message) extends Command
case class TwoPCAbort(m: Message) extends Command


//Testing
case object PassTest extends Command
case object FailTest extends Command