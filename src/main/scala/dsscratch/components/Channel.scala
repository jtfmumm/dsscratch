package dsscratch.components

import scala.collection.mutable.Queue

trait Channel extends Steppable {
  val id = -1
  var failed = false
  def recv(m: Message): Unit
  def hasSource(p: Process): Boolean
  def hasTarget(p: Process): Boolean
  def containsPath(p0: Process, p1: Process) = hasSource(p0) && hasTarget(p1)
  def fail(): Unit = failed = true
  def restart(): Unit = failed = false
}

case class TwoChannel(p0: Process, p1: Process, override val id: Int = -1) extends Channel {
  val msgs = Queue[Message]()

  def recv(m: Message): Unit = {
    if (failed) return
    assert(m.sender == p0, m.sender + " can't send over " + this)
    msgs.enqueue(m)
  }

  def step(): Unit = {
    if (!failed) deliverNext()
  }

  def deliverNext(): Unit = {
    if (msgs.isEmpty) return

    p1.recv(msgs.dequeue())
  }

  def hasSource(p: Process) = {
    p0 == p
  }

  def hasTarget(p: Process) = {
    p1 == p
  }

  override def toString: String = "Channel " + id + ": " + p0 + " -> " + p1
}

case class MultiChannel(ps: Seq[Process], override val id: Int = -1) extends Channel {
  val msgs = Queue[Message]()

  def recv(m: Message) = {
    assert(ps.contains(m.sender), m.sender + " can't send over " + this)
    msgs.enqueue(m)
  }

  def step(): Unit = deliverNext()

  def deliverNext(): Unit = {
    if (msgs.isEmpty) return

    val nextM = msgs.dequeue()
    val sender = nextM.sender
    for (nd: Process <- ps.filter(_ != sender)) {
      nd.recv(nextM)
    }
  }

  def hasSource(p: Process) = ps.contains(p)

  def hasTarget(p: Process) = ps.contains(p)

  def is(ch: Channel): Boolean = ch match {
    case c: MultiChannel => ps.forall(x => c.ps.contains(x))
    case _ => false
  }

  override def toString: String = "MultiChannel " + id + ": " + ps.mkString(", ")
}

object Channel {
  val empty = TwoChannel(EmptyProcess(), EmptyProcess())
}
