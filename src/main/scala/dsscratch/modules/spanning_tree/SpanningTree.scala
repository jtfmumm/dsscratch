package dsscratch.modules.spanning_tree

import dsscratch.clocks.TimeStamp
import dsscratch.components._

////////////
// Commands
////////////
case class Read(key: String) extends Command
case class ReadReply(v: Int) extends Command

case class RequestUpdate(key: String, v: Int) extends Command
case class Update(key: String, v: Int) extends Command

case class RequestDelete(key: String) extends Command
case class Delete(key: String) extends Command

// Tarry
case class Token(id: ProcessId)

case class EmptyCommand() extends Command

case class IntToken(t: Token) extends Command

case class ProcessToken(t: Token) extends Command
