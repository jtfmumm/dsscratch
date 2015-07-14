package dsscratch

import scala.collection.mutable.{Map => mMap}
import scala.collection.mutable.Queue



case class Dictionary() {
  var d = mMap[String, Int]()

  def get(k: String) = d.getOrElse(k, 0)

  def put(k: String) = d(k) = 1

  def delete(k: String) = d(k) = 0
}


trait TimeStamp

case class Count(value: Int) extends TimeStamp


trait Message

case class Read(key: String, sender: Node, timestamp: TimeStamp) extends Message

case class Update(key: String, sender: Node, timestamp: TimeStamp) extends Message

case class Delete(key: String, sender: Node, timestamp: TimeStamp) extends Message


case class Channel {
  type Target = Node
  def q = Queue[(Target, Message)]()

  def schedule(t: Target, m: Message) = q.enqueue((t, m))

  def deliverNext: Unit = {
    if (q.isEmpty) return

    val next = q.dequeue()
    next._1.recv(next._2)
  }
}


case class Node {
  val data = new Dictionary()

  def send(ch: Channel, target: Node, m: Message) = ch.schedule(target, m)

  def recv(m: Message) = m match {
    case Read(k, s, t) => data.get(k)
    case Update(k, s, t) => data.put(k)
    case Delete(k, s, t) => data.delete(k)
  }
}

