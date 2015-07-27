package dsscratch.algos.dict

import dsscratch.components._
import dsscratch.clocks._
import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.dict.Dictionary
import scala.collection.mutable.Queue


// Distributed key value dictionary
// At the moment, this algorithm will fail to
// coordinate properly as it naively just has
// each node pass transactions along.

trait DictLocalState extends LocalState {
  var dict: Dictionary
  var nextCommands: Queue[Command]
}

object DictComponent {
  def apply(parentProcess: Process): DictComponent = {
    new DictComponent(parentProcess)
  }
  def buildWith(parentProcess: Process, s: DictLocalState): DictComponent = {
    val newC = DictComponent(parentProcess)

    newC.s.dict = s.dict.clone()
    newC.s.nextCommands = s.nextCommands.map(x => x).toQueue
    newC
  }
}

class DictComponent(val parentProcess: Process, isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.DICT
  val outChs = parentProcess.outChs
  val inChs = parentProcess.inChs

  ////////////////////
  //LOCAL STATE
  private object s extends DictLocalState {
    var dict = Dictionary()
    var nextCommands = Queue[Command]()
  }
  ////////////////////

  def processMessage(m: Message): Unit = m.cmd match {
    case Read(k) => {
      val reply = ReadReply(s.dict.get(k))
      val ch: Channel = inChs.filter(_.hasSource(m.sender))(0)
      val msg = Message(reply, parentProcess, clock.stamp())
      log.write("[Dict] Sending on " + ch, msg.ts)
      ch.recv(msg)
    }
    case Update(k, v) => {
      try {
        s.dict.put(k, v)
        s.nextCommands.enqueue(m.cmd)
      }
      catch {
        case _: Throwable =>
      }
    }
    case Delete(k) => {
      s.dict.delete(k)
      s.nextCommands.enqueue(m.cmd)
    }
    case _ =>
  }

  def terminated: Boolean = false

  def step(): Unit = {
    if (parentProcess.failed) return
    if (terminated) return
    if (s.nextCommands.nonEmpty) {
      val next = s.nextCommands.dequeue()
      val msg = Message(next, parentProcess, clock.stamp())
      outChs.foreach(_.recv(msg))
    }
  }

  def snapshot: DictComponent = DictComponent.buildWith(parentProcess, s)

  def result = s.dict.toString
}
