package dsscratch.modules.heartbeat

import dsscratch.clocks.TimeStamp
import dsscratch.components._

////////////
// Commands
////////////
case object SendHeartbeat extends Command
case class Heartbeat(senderId: ProcessId, ts: TimeStamp) extends Command
