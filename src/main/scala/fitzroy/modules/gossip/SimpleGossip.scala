package fitzroy.modules.gossip

import fitzroy.modules._
import fitzroy.components._
import fitzroy.clocks._
import fitzroy.util.Rand

import scala.collection.mutable.ArrayBuffer

/*
Simple gossip protocol. Every step it rolls a die to determine whether
or not to send a StartGossip command to its parent process so that other
components will broadcast any data that should be spread to other nodes
via gossip.
*/

trait SimpleGossipLocalState extends LocalState {}

object SimpleGossipModule extends ModuleBuilder {
  def apply(parent: ModuleParent, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): SimpleGossipModule = {
    new SimpleGossipModule(parent)
  }
  def buildWith(parent: ModuleParent, s: SimpleGossipLocalState):
    SimpleGossipModule = {
    val newC = new SimpleGossipModule(parent)
    newC
  }
}

class SimpleGossipModule(val parent: ModuleParent) extends Module {
  val moduleCode: ModuleCode = ModuleCodes.SIMPLE_GOSSIP

  ////////////////////
  //LOCAL STATE
  private object s extends SimpleGossipLocalState {}
  ////////////////////

  def processMessage(cmd: Command, chSenderId: ProcessId, ts: TimeStamp): Unit = {
    // Nothing to do
  }

  def terminated: Boolean = false //never terminates

  def step(): Unit = {
    if (Rand.roll(10) < 2)
      parent.deliver(StartGossip)
  }

  def snapshot: SimpleGossipModule =
    SimpleGossipModule.buildWith(parent, s)

  def result = "" // For printing results
}
