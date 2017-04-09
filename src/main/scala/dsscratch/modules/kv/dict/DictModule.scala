package dsscratch.modules.kv.dict

import dsscratch.components._
import dsscratch.clocks._
import dsscratch.modules._
import dsscratch.modules.consensus._
import dsscratch.modules.kv._
import dsscratch.datast.dict.Dictionary
import scala.collection.mutable.Queue


// Distributed key value dictionary
// The algorithm receives requests and
// tries to commit them, relaying on the
// available commit/consensus protocol.

trait DictLocalState extends LocalState {
  var dict: Dictionary
}

object DictModule extends ModuleBuilder {
  def apply(parent: ModuleParent, nodeIds: Set[ProcessId],
    isInitiator: Boolean = false): DictModule = {
    new DictModule(parent)
  }
  def buildWith(parent: ModuleParent, s: DictLocalState): DictModule = {
    val newC = new DictModule(parent)

    newC.s.dict = s.dict.clone()
    newC
  }
}

class DictModule(val parent: ModuleParent)
  extends Module {
  val moduleCode: ModuleCode = ModuleCodes.DICT

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
        log.write("[Dict] Sending to Node" + targetId, clock.stamp())
        parent.send(targetId, reply)
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
    val commit = Commit(Update(k, v), parent.id, clock.stamp())
    parent.deliver(commit)
  }

  private def requestDelete(k: String) = {
    val commit = Commit(Delete(k), parent.id, clock.stamp())
    parent.deliver(commit)
  }

  def snapshot: DictModule = DictModule.buildWith(parent, s)

  def result = parent + ": " + s.dict.toString + "\n"
}
