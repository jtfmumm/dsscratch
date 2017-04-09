package dsscratch.modules.crdt

import dsscratch.clocks.TimeStamp
import dsscratch.components._
import dsscratch.datast.crdt._


////////////
// Commands
////////////

//PCounter
case class MergePCounter(pCounter: PCounter) extends Command
case object IncrementPCounter extends Command
case class UpdatedPCounter(pCounter: PCounter) extends Command
