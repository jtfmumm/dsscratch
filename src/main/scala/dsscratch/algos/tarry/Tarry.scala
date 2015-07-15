package dsscratch.algos.tarry

import dsscratch.components._
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer


//Tarry's algorithm
//Kicked off by one initiator
//1) A process never forwards the token through the same channel twice
//2) A process only forwards the token to its parent when there is no other option


case class Token(id: Int)

case class ProcessToken(t: Token, ch: Channel) extends Command


case class TNode extends Process {
  val chs = ArrayBuffer[Channel]()
  val tokens = Queue[Token]()

  var parentCh: Channel = Channel.empty
  var nonParentChsToSend = ArrayBuffer[Channel]()

  def recv(m: Message): Unit = m.cmd match {
    case pt: ProcessToken => processToken(pt)
    case _ =>
  }

  def step(): Unit = {
    if (tokens.isEmpty) return
    nonParentChsToSend.size match {
      case 0 if hasNoParent =>
      case 0 => {
        sendToken(parentCh)
        emptyParent()
      }
      case _ => {
        val randChIndex = Math.random
        val ch = nonParentChsToSend.remove(0)
        sendToken(ch)
      }
    }
  }

  def addChannel(ch: Channel): Unit = chs.append(ch)

  def removeChannel(ch: Channel): Unit = {
    val i = chs.indexOf(ch)
    chs.remove(i)
  }

  def initiate(): Unit = {}

  private def hasNoParent: Boolean = parentCh == Channel.empty

  private def sendToken(ch: Channel) = {
    val pt = ProcessToken(tokens.last, ch)
    ch.recv(Message(pt, this))
  }

  private def processToken(pt: ProcessToken): Unit = {
    if (tokens.isEmpty) {
      parentCh = pt.ch
      nonParentChsToSend = ArrayBuffer(chs.filter(_ != parentCh): _*)
    }
    tokens.enqueue(pt.t)
  }

  private def emptyChsToSend(): Unit = {
    nonParentChsToSend = ArrayBuffer[Channel]()
  }

  private def emptyParent(): Unit = {
    parentCh = Channel.empty
  }
}