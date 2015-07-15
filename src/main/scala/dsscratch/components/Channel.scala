package dsscratch.components

import scala.collection.mutable.Queue

trait Channel {
  def recv(m: Message): Unit
  def deliverNext(): Unit
}

case class TwoChannel(p0: Process, p1: Process) extends Channel {
  val msgs = Queue[Message]()

  def recv(m: Message) = {
    assert(m.sender == p0 || m.sender == p1)
    msgs.enqueue(m)
  }

  def deliverNext(): Unit = {
    if (msgs.isEmpty) return

    val nextM = msgs.dequeue()
    nextM.sender match {
      case a if a == p0 => p1.recv(nextM)
      case b if b == p1 => p0.recv(nextM)
    }
  }
}

case class MultiChannel(ps: Seq[Process]) extends Channel {
  val msgs = Queue[Message]()

  def recv(m: Message) = {
    assert(ps.contains(m.sender))
    msgs.enqueue(m)
  }

  def deliverNext(): Unit = {
    if (msgs.isEmpty) return

    val nextM = msgs.dequeue()
    val sender = nextM.sender
    for (nd: Process <- ps.filter(_ != sender)) {
      nd.recv(nextM)
    }
  }
}

object Channel {
  val empty = TwoChannel(EmptyProcess(), EmptyProcess())
}
