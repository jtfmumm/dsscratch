package dsscratch.algos.dict

import dsscratch.components._
import dsscratch.clocks._
import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.dict.Dictionary


// Use this empty example as a template for creating new components.
// Components encapsulate algorithms and the related local state.

trait DictLocalState extends LocalState {
  var data = Dictionary()
  //state fields...
}

object DictComponent {
  def apply(parentProcess: Process, isInitiator: Boolean = false): DictComponent = {
    new DictComponent(parentProcess, isInitiator)
  }
  def buildWith(parentProcess: Process, s: DictLocalState): DictComponent = {
    val newC = DictComponent(parentProcess, s.initiator)

    newC.s.initiated = s.initiated
    //newC.s.x = ...
    newC
  }
}

class DictComponent(val parentProcess: Process, isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.NONE // <-- use code
  val outChs = parentProcess.outChs
  val inChs = parentProcess.inChs

  ////////////////////
  //LOCAL STATE
  private object s extends DictLocalState {
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

  def processMessage(m: Message): Unit = m.cmd match {
    case Read(k) => {
      val reply = ReadReply(data.get(k))
      val ch: Channel = inChs.filter(_.hasSource(m.sender))(0)
      send(Message(reply, parentProcess, clock.stamp()), ch)
    }
    case Update(k, v) => data.put(k, v)
    case Delete(k) => data.delete(k)
    case _ =>
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

  def snapshot: DictComponent = DictComponent.buildWith(parentProcess, s)

  def result = "" // For printing results
}
