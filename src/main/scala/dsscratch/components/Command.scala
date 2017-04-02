package dsscratch.components

import dsscratch.clocks.TimeStamp
import dsscratch.client_server._
import dsscratch.algos.crdt.p_counter._

trait Command

// This mess of commands needs to be broken out


case class Token(id: ProcessId)


case class EmptyCommand() extends Command

case class IntToken(t: Token) extends Command

case class ProcessToken(t: Token) extends Command

case class TakeSnapshot(snapId: TimeStamp) extends Command

////CRUD
case class Read(key: String) extends Command
case class ReadReply(v: Int) extends Command

case class RequestUpdate(key: String, v: Int) extends Command
case class Update(key: String, v: Int) extends Command

case class RequestDelete(key: String) extends Command
case class Delete(key: String) extends Command

////Broadcast

//General broadcast command
case class Broadcast(cmd: Command, senderId: ProcessId, ts: TimeStamp)
  extends Command

case class SimpleBroadcast(cmd: Command, senderId: ProcessId, ts: TimeStamp) extends Command

case class InitiateEcho(cmd: Command, senderId: ProcessId, ts: TimeStamp) extends Command
case class Echo(cmd: Command, senderId: ProcessId, ts: TimeStamp)
  extends Command

////Consensus

//General commit command
case class Commit(cmd: Command, senderId: ProcessId, ts: TimeStamp)
  extends Command


//2PC
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

////ClientConnection
case class Request(cmd: Command, conn: ClientConnection) extends Command
case class Response(cmd: Command) extends Command

////Gossip
case object StartGossip extends Command

//PCounter
case class MergePCounter(pCounter: PCounter) extends Command
case object IncrementPCounter extends Command
case class UpdatedPCounter(pCounter: PCounter) extends Command

//Testing
case object PassTest extends Command
case object FailTest extends Command
