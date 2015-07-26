package dsscratch.algos.nodes

import dsscratch.components._
import dsscratch.clocks._
import dsscratch.algos._
import dsscratch.algos.nodes._


// Use this empty example as a template for creating new components.
// Components encapsulate algorithms and the related local state.

trait NCLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
  //state fields...
}

object NCComponent {
  def apply(parentProcess: Process, isInitiator: Boolean = false): NCComponent = {
    new NCComponent(parentProcess, isInitiator)
  }
  def buildWith(parentProcess: Process, s: NCLocalState): NCComponent = {
    val newC = NCComponent(parentProcess, s.initiator)

    newC.s.initiated = s.initiated
    //newC.s.x = ...
    newC
  }
}

class NCComponent(val parentProcess: Process, isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.NONE // <-- use code
  val outChs = parentProcess.outChs
  val inChs = parentProcess.inChs

  ////////////////////
  //LOCAL STATE
  private object s extends NCLocalState {
    var initiated = false
    var initiator: Boolean = isInitiator
    //set other fields...
  }
  ////////////////////

  def processMessage(m: Message): Unit = {
    m.cmd match {
      //case c @ RelevantCommand(_) => //Do something with commands relevant to the algorithm
      case _ => // Ignore the rest
    }
  }

  def terminated: Boolean = false //set termination condition

  def step(): Unit = {
    if (parentProcess.failed) return
    if (s.initiator && !s.initiated) initiate()
    if (terminated) return
    // Do something each step...
  }

  def initiate(): Unit = {
    //Do some initiation work...
    s.initiated = true
  }

  def snapshot: NCComponent = NCComponent.buildWith(parentProcess, s)

  def result = "" // For printing results
}
