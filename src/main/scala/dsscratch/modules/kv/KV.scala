package dsscratch.modules.kv

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
