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

object DictModule extends NodeModuleBuilder {
  def apply(parentNode: Node, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): DictModule = {
    new DictModule(parentNode)
  }
  def buildWith(parentNode: Node, s: DictLocalState): DictModule = {
    val newC = new DictModule(parentNode)

    newC.s.dict = s.dict.clone()
    newC
  }
}

class DictModule(val parentNode: Node)
  extends NodeModule {
  val algoCode: AlgoCode = AlgoCodes.DICT

  ////////////////////
  //LOCAL STATE
  private object s extends DictLocalState {
    var dict = Dictionary()
  }
  ////////////////////

  def processMessage(cmd: Command, chSenderId: ProcessId, ts: TimeStamp):
    Unit = {
    cmd match {
      case Read(k) => {
        val reply = ReadReply(s.dict.get(k))
        val targetId = inProcessIds.filter(_ == chSenderId).head
        val msg = Message(reply, parentNode.id, clock.stamp())
        log.write("[Dict] Sending to Node" + targetId, msg.ts)
        parentNode.send(targetId, msg)
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
  }

  def terminated: Boolean = s.dict.keys.nonEmpty

  def step(): Unit = {
    if (terminated) return
    // nothing to do...
  }

  private def requestUpdate(k: String, v: Int) = {
    val commit = Commit(Update(k, v), parentNode.id, clock.stamp())
    parentNode.deliver(Message(commit, parentNode.id, clock.stamp()))
  }

  private def requestDelete(k: String) = {
    val commit = Commit(Delete(k), parentNode.id, clock.stamp())
    parentNode.deliver(Message(commit, parentNode.id, clock.stamp()))
  }

  def snapshot: DictModule = DictModule.buildWith(parentNode, s)

  def result = parentNode + ": " + s.dict.toString + "\n"
}
