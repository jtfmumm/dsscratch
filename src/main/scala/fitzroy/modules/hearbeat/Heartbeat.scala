package fitzroy.modules.heartbeat

import fitzroy.clocks.TimeStamp
import fitzroy.components._

////////////
// Commands
////////////
case object SendHeartbeat extends Command
case class Heartbeat(senderId: ProcessId, ts: TimeStamp) extends Command
