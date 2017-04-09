package fitzroy.modules.broadcast

import fitzroy.clocks.TimeStamp
import fitzroy.components._

////////////
// Commands
////////////
case class Broadcast(cmd: Command, senderId: ProcessId, ts: TimeStamp)
  extends Command

// For implementations
case class AckBroadcast(cmd: Command, senderId: ProcessId, ts: TimeStamp)
  extends Command

case class SimpleBroadcast(cmd: Command, senderId: ProcessId, ts: TimeStamp)
  extends Command

case class InitiateEcho(cmd: Command, senderId: ProcessId, ts: TimeStamp)
  extends Command

case class Echo(cmd: Command, senderId: ProcessId, ts: TimeStamp)
  extends Command
