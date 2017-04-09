package dsscratch.modules.snapshots

import dsscratch.clocks.TimeStamp
import dsscratch.components._

case class Snapshot(id: TimeStamp, nd: Node)


////////////
// Commands
////////////
case class TakeSnapshot(snapId: TimeStamp) extends Command
