package dsscratch.components

import scala.collection.mutable.Queue


case class Channel(p0: Process, p1: Process) {
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
