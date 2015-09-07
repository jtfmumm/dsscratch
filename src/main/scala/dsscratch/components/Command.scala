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