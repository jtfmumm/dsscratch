package dsscratch.dict

import dsscratch.components._
import scala.collection.mutable.{Map => mMap}
import scala.collection.mutable.Queue

case class Dictionary() {
  var d = mMap[String, Int]()

  def get(k: String) = d.getOrElse(k, 0)

  def put(k: String) = d(k) = 1

  def delete(k: String) = d(k) = 0
}


// COMMANDS
case class Read(key: String, ch: Channel) extends Command
case class ReadReply(v: Any) extends Command

case class Update(key: String) extends Command

case class Delete(key: String) extends Command


// PROCESSES
case class DictNode extends Process {
  val data = new Dictionary()
  val msgs = Queue[Message]()

  // send is implemented on Process

  def recv(m: Message) = msgs.enqueue(m)

  def step(): Unit = {
    if (msgs.isEmpty) return
    val nextCommand = msgs.dequeue().cmd
    process(nextCommand)
  }

  def process(c: Command): Unit = c match {
    case Read(k, ch) => {
      val reply = ReadReply(data.get(k))
      send(Message(reply, this, tStamp), ch)
    }
    case Update(k) => data.put(k)
    case Delete(k) => data.delete(k)
  }

  def tStamp: Count = Count(0)
}