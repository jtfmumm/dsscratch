package dsscratch.modules

import dsscratch.clocks._
import dsscratch.components._

import scala.collection.mutable.ArrayBuffer

/*
Use this empty example as a template for creating new modules.
Modules encapsulate algorithms and the related local state.
Find and replace ZZ with your component name in your copy of this file.

The parent Node of the component handles clock updates based on messages
received, so don't do that here.
*/

trait ZZLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
  //state fields...
}

object ZZModule extends ModuleBuilder {
  def apply(parent: ModuleParent, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): ZZModule = {
    new ZZModule(parent, isInitiator)
  }
  def buildWith(parent: ModuleParent, s: ZZLocalState): ZZModule = {
    val newC = new ZZModule(parent, s.initiator)

    newC.s.initiated = s.initiated
    //newC.s.x = ...
    newC
  }
}

class ZZModule(val parent: ModuleParent, isInitiator: Boolean = false) extends Module {
  val moduleCode: ModuleCode = ModuleCodes.NONE // <-- use code
  // A Module can access the ids of Nodes it has channels to and
  // Nodes that have channels to its parent via the following methods
  // on the Module trait:
  // -- outProcessIds
  // -- inProcessIds

  ////////////////////
  //LOCAL STATE
  private object s extends ZZLocalState {
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

  def snapshot: ZZModule = ZZModule.buildWith(parent, s)

  def result = "" // For printing results

  // If you need to trigger the algo on an initiator
  private def initiate(): Unit = {
    //Do some initiation work...
    s.initiated = true
  }
}
