package dsscratch.algos.nodes

import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.components._
import dsscratch.clocks._
import dsscratch.util.Rand

import scala.collection.mutable.ArrayBuffer

/*
Simple gossip protocol. Every step it rolls a die to determine whether
or not to send a StartGossip command to its parent process so that other
components will broadcast any data that should be spread to other nodes
via gossip.
*/

trait SimpleGossipLocalState extends LocalState {}

object SimpleGossipModule extends NodeModuleBuilder {
  def apply(parentNode: Node, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): SimpleGossipModule = {
    new SimpleGossipModule(parentNode)
  }
  def buildWith(parentNode: Node, s: SimpleGossipLocalState):
    SimpleGossipModule = {
    val newC = new SimpleGossipModule(parentNode)
    newC
  }
}

class SimpleGossipModule(val parentNode: Node) extends NodeModule {
  val algoCode: AlgoCode = AlgoCodes.SIMPLE_GOSSIP

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
      parentNode.deliver(Message(StartGossip, parentNode.id, clock.stamp()))
  }

  def snapshot: SimpleGossipModule =
    SimpleGossipModule.buildWith(parentNode, s)

  def result = "" // For printing results
}
