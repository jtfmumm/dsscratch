package fitzroy.modules.crdt

import fitzroy.clocks.TimeStamp
import fitzroy.components._
import fitzroy.datast.crdt._


////////////
// Commands
////////////

//PCounter
case class MergePCounter(pCounter: PCounter) extends Command
case object IncrementPCounter extends Command
case class UpdatedPCounter(pCounter: PCounter) extends Command
