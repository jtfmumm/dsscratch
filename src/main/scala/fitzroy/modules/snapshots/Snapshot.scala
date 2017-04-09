package fitzroy.modules.snapshots

import fitzroy.clocks.TimeStamp
import fitzroy.components._

case class Snapshot(id: TimeStamp, nd: Node)


////////////
// Commands
////////////
case class TakeSnapshot(snapId: TimeStamp) extends Command
