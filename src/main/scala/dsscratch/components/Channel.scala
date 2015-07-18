package dsscratch.components

import scala.collection.mutable.Queue

trait Channel {
  def recv(m: Message): Unit
  def step(): Unit
  def hasTarget(p: Process): Boolean
//  def is(ch: Channel): Boolean
}

case class TwoChannel(p0: Process, p1: Process, id: Int = 0) extends Channel {
  val msgs = Queue[Message]()

  def recv(m: Message) = {
    assert(m.sender == p0, m.sender + " can't send over " + this)
    msgs.enqueue(m)
  }

  def step(): Unit = deliverNext()

  def deliverNext(): Unit = {
    if (msgs.isEmpty) return

    p1.recv(msgs.dequeue())
  }

  def hasTarget(p: Process) = {
    p1 == p
  }

  override def toString: String = "Channel " + id + ": " + p0 + " -> " + p1
}

case class MultiChannel(ps: Seq[Process], id: Int = 0) extends Channel {
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
