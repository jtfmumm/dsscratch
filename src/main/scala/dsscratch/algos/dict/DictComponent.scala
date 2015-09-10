package dsscratch.algos.dict

import dsscratch.components._
import dsscratch.clocks._
import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.dict.Dictionary
import scala.collection.mutable.Queue


// Distributed key value dictionary
// The algorithm receives requests and
// tries to commit them, relaying on the
// available commit/consensus protocol.

trait DictLocalState extends LocalState {
  var dict: Dictionary
}

object DictComponent {
  def apply(parentProcess: Process): DictComponent = {
    new DictComponent(parentProcess)
  }
  def buildWith(parentProcess: Process, s: DictLocalState): DictComponent = {
    val newC = DictComponent(parentProcess)

    newC.s.dict = s.dict.clone()
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
    case RequestUpdate(k, v) => requestUpdate(k, v)
    case Update(k, v) => {
      try {
        s.dict.put(k, v)
      }
      catch {
        case _: Throwable =>
      }
    }
    case RequestDelete(k) => requestDelete(k)
    case Delete(k) => {
      s.dict.delete(k)
    }
    case _ =>
  }

  def terminated: Boolean = s.dict.keys.nonEmpty

  def step(): Unit = {
    if (parentProcess.failed) return
    if (terminated) return
    // nothing to do...
  }

  private def requestUpdate(k: String, v: Int) = {
    val commit = Commit(Update(k, v), parentProcess, clock.stamp())
    parentProcess.recv(Message(commit, parentProcess, clock.stamp()))
  }

  private def requestDelete(k: String) = {
    val commit = Commit(Delete(k), parentProcess, clock.stamp())
    parentProcess.recv(Message(commit, parentProcess, clock.stamp()))
  }

  def snapshot: DictComponent = DictComponent.buildWith(parentProcess, s)

  def result = parentProcess + ": " + s.dict.toString + "\n"
}
