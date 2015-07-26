package dsscratch.dict

import dsscratch.clocks.TS
import dsscratch.components._
import dsscratch.clocks.LamportClock
import scala.collection.mutable.{Map => mMap}
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer

case class Dictionary() {
  var d = mMap[String, Int]()

  def get(k: String) = d.getOrElse(k, -1)

  def put(k: String, v: Int) = d(k) = v

  def delete(k: String) = d(k) = -1
}

object Dictionary {
  def apply(): Dictionary = new Dictionary()
}


// COMMANDS



// PROCESSES
case class DictNode(id: Int = 0) extends Process {
  override val clock = LamportClock(id)
  val chs = ArrayBuffer[Channel]()
  val data = new Dictionary()
  val msgs = Queue[Message]()

  // send is implemented on Process

  def recv(m: Message) = msgs.enqueue(m)

  def step(): Unit = {
    if (msgs.isEmpty) return
    val nextCommand = msgs.dequeue().cmd
    process(nextCommand)
  }

  private def process(c: Command): Unit = c match {
    case Read(k, ch) => {
      val reply = ReadReply(data.get(k))
      send(Message(reply, this, tStamp), ch)
    }
    case Update(k, v) => data.put(k, v)
    case Delete(k) => data.delete(k)
  }

  def addChannel(ch: Channel): Unit = chs.append(ch)

  def removeChannel(ch: Channel): Unit = {
    val i = chs.indexOf(ch)
    chs.remove(i)
  }

  def initiate(): Unit = {}

  private def tStamp: TS = TS(0, id)
}