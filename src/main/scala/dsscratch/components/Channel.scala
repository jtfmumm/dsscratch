package dsscratch.components

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue

trait Channel extends Steppable {
  val id: Int
  var failed = false
  var stepsDelayed = 0
  def recv(m: Message): Unit
  def sourceIds: Set[ProcessId]
  def targetIds: Set[ProcessId]
  def hasSource(p: Process): Boolean
  def hasSourceId(id: ProcessId): Boolean
  def hasTarget(p: Process): Boolean
  def hasTargetId(id: ProcessId): Boolean
  def containsPath(p0: Process, p1: Process) = hasSource(p0) && hasTarget(p1)
  def fail(): Unit = failed = true
  def restart(): Unit = failed = false
  def delay(steps: Int) = stepsDelayed += steps
}

case class TwoChannel(p0: Process, p1: Process, val id: Int) extends Channel {
  val msgs = Queue[Message]()

  def recv(m: Message): Unit = {
    if (failed) return
    assert(m.senderId == p0.id, "Node " + m.senderId + " can't send over " +
      this)
    msgs.enqueue(m)
  }

  def step(): Unit = if (!failed) deliverNext()

  def deliverNext(): Unit = {
    if (stepsDelayed > 0) {
      println("!!Delayed")
      stepsDelayed -= 1
      return
    }
    if (msgs.isEmpty) return

    p1.recv(msgs.dequeue())
  }

  def sourceIds: Set[ProcessId] = Set(p0.id)

  def targetIds: Set[ProcessId] = Set(p1.id)

  def hasSource(p: Process): Boolean = p0 == p

  def hasSourceId(id: ProcessId): Boolean = p0.id == id

  def hasTarget(p: Process): Boolean = p1 == p

  def hasTargetId(id: ProcessId): Boolean = p1.id == id

  override def toString: String = "Channel " + id + ": " + p0 + " -> " + p1
}

case class MultiChannel(ps: Set[Process], val id: Int) extends Channel {
  val msgs = Queue[Message]()

  def recv(m: Message): Unit = {
    if (failed) return
    assert(ps.filter(_.id == m.senderId).nonEmpty, "Node" + m.senderId +
      " can't send over " + this)
    msgs.enqueue(m)
  }

  def step(): Unit = if (!failed) deliverNext()

  def deliverNext(): Unit = {
    if (stepsDelayed > 0) {
      stepsDelayed -= 1
      return
    }
    if (msgs.isEmpty) return

    val nextM = msgs.dequeue()
    val senderId = nextM.senderId
    for (nd: Process <- ps.filter(_.id != senderId)) {
      nd.recv(nextM)
    }
  }

  def sourceIds: Set[ProcessId] = ps.map(_.id)

  def targetIds: Set[ProcessId] = ps.map(_.id)

  def hasSource(p: Process): Boolean = ps.contains(p)

  def hasSourceId(id: ProcessId): Boolean = ps.map(_.id).contains(id)

  def hasTarget(p: Process): Boolean = ps.contains(p)

  def hasTargetId(id: ProcessId): Boolean = ps.map(_.id).contains(id)

  def is(ch: Channel): Boolean = ch match {
    case c: MultiChannel => ps.forall(x => c.ps.contains(x))
    case _ => false
  }

  override def toString: String = "MultiChannel " + id + ": " + ps.mkString(", ")
}

object Channel {
  val empty = TwoChannel(EmptyProcess, EmptyProcess, -1)
}
