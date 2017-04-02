package dsscratch.components

import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.clocks._

import scala.collection.mutable.ArrayBuffer

/*
Use this empty example as a template for creating new components.
Modules encapsulate algorithms and the related local state.
Find and replace NC with your component name in your copy of this file.

The parent Node of the component handles clock updates based on messages
received, so don't do that here.
*/

trait NCLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
  //state fields...
}

object NCModule extends NodeModuleBuilder {
  def apply(parentNode: Node, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): NCModule = {
    new NCModule(parentNode, isInitiator)
  }
  def buildWith(parentNode: Node, s: NCLocalState): NCModule = {
    val newC = new NCModule(parentNode, s.initiator)

    newC.s.initiated = s.initiated
    //newC.s.x = ...
    newC
  }
}

class NCModule(val parentNode: Node, isInitiator: Boolean = false) extends NodeModule {
  val algoCode: AlgoCode = AlgoCodes.NONE // <-- use code
  // A NodeModule can access the ids of Nodes it has channels to and
  // Nodes that have channels to its parent via the following methods
  // on the NodeModule trait:
  // -- outProcessIds
  // -- inProcessIds

  ////////////////////
  //LOCAL STATE
  private object s extends NCLocalState {
    // Optional state fileds for keeping track of initiation
    var initiated: Boolean = false
    var initiator: Boolean = isInitiator
    //set other fields...
  }
  ////////////////////

  def processMessage(cmd: Command, chSenderId: ProcessId, ts: TimeStamp):
    Unit =
  {
    cmd match {
      //case c @ RelevantCommand(_) => //Do something with commands relevant to the algorithm
      case _ => // Ignore the rest
    }
  }

  def terminated: Boolean = false //set termination condition

  def step(): Unit = {
    // If you have an initiator, you can trigger initiation here
    if (s.initiator && !s.initiated) initiate()
    if (terminated) return
    // Do something each step...
  }

  def snapshot: NCModule = NCModule.buildWith(parentNode, s)

  def result = "" // For printing results

  // If you need to trigger the algo on an initiator
  private def initiate(): Unit = {
    //Do some initiation work...
    s.initiated = true
  }
}
