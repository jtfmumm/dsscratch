package dsscratch.components

import scala.collection.mutable.Queue

trait Channel {
  def recv(m: Message): Unit
  def step(): Unit
  def hasSource(p: Process): Boolean
  def hasTarget(p: Process): Boolean
  def containsPath(p0: Process, p1: Process) = hasSource(p0) && hasTarget(p1)
}

case class TwoChannel(p0: Process, p1: Process, id: Int = -1) extends Channel {
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

  def hasSource(p: Process) = {
    p0 == p
  }

  def hasTarget(p: Process) = {
    p1 == p
  }

  override def toString: String = "Channel " + id + ": " + p0 + " -> " + p1
}

case class MultiChannel(ps: Seq[Process], id: Int = -1) extends Channel {
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

//case class TwoChannels(chs: ArrayBuffer[TwoChannel] = ArrayBuffer[TwoChannel]()) {
//  def containsPath(p0: Process, p1: Process) = chs.exists(ch => ch.hasSource(p0) && ch.hasTarget(p1))
//
//  def append(ch: TwoChannel) = chs.append(ch)
//  def remove(ch: TwoChannel) = {
//    val idx = chs.indexOf(ch)
//    chs.remove(idx)
//  }
//
//  def data: ArrayBuffer[TwoChannel] = chs
//
//  def map[B](f: TwoChannel => B): ArrayBuffer[B] = chs.map(f)
//  def flatMap[B](f: TwoChannel => GenTraversableOnce[B]): ArrayBuffer[B] = chs.flatMap(f)
//  def filter[A](f: TwoChannel => Boolean): ArrayBuffer[TwoChannel] = chs.filter(f)
//  def foreach[A](f: TwoChannel => Unit): Unit = chs.foreach(f)
//
//  def dotGraph: String = {
//    "digraph G {\n" +
//    "  concentrate=true;\n" +
//    chs.map(ch => "  " + ch.p0 + " -> " + ch.p1 + ";\n").mkString +
//    "}"
//  }
//}
//
//case class MultiChannels(chs: ArrayBuffer[MultiChannel] = ArrayBuffer[MultiChannel]()) {
//  def containsPath(p0: Process, p1: Process) = chs.exists(ch => ch.hasSource(p0) && ch.hasTarget(p1))
//
//  def append(ch: MultiChannel) = chs.append(ch)
//  def remove(ch: MultiChannel) = {
//    val idx = chs.indexOf(ch)
//    chs.remove(idx)
//  }
//
//  def data: ArrayBuffer[MultiChannel] = chs
//
//  def map[B](f: MultiChannel => B): ArrayBuffer[B] = chs.map(f)
//  def flatMap[B](f: MultiChannel => GenTraversableOnce[B]): ArrayBuffer[B] = chs.flatMap(f)
//  def filter[A](f: MultiChannel => Boolean): ArrayBuffer[MultiChannel] = chs.filter(f)
//  def foreach[A](f: MultiChannel => Unit): Unit = chs.foreach(f)
//
//  def dotGraph: String = {
//    "No dot graph available."
//  }
//}