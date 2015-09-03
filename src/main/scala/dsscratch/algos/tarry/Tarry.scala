package dsscratch.algos.tarry

import dsscratch.components._
import dsscratch.clocks._
import randomific.Rand
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer

//Tarry's algorithm
//Kicked off by one initiator
//1) A process never forwards the token through the same channel twice
//2) A process only forwards the token to its parent when there is no other option


// This version isn't based on NodeComponents,
// so it isn't flexible enough to be run simultaneously
// with other algorithms.

// See TarryComponent for the more flexible version.

case class TNode(id: Int, initiator: Boolean = false) extends Process {
  override val clock = LamportClock(id)
  var initiated = false
  val chs = ArrayBuffer[Channel]()
  val tokens = Queue[Token]()
  val finishedTokens = Queue[Token]()
  log.write(this + " log", clock.stamp())

  var parent: Process = EmptyProcess()
  var parentCh: Channel = Channel.empty
  var nonParentChsToSend = ArrayBuffer[Channel]()

  def recv(m: Message): Unit = {
    if (failed) return
    clock.compareAndUpdate(m.ts)
    log.write("Receiving " + m, clock.stamp())
    m.cmd match {
      case ProcessToken(t) => processToken(t, m.sender)
      case _ =>
    }
  }

  def step(): Unit = {
    if (failed) return
    if (initiator && !initiated) initiate()
    if (tokens.isEmpty && finishedTokens.isEmpty) return
    nonParentChsToSend.size match {
      case 0 if hasNoParent && tokens.nonEmpty => {
        val t = tokens.dequeue()
        finishedTokens.enqueue(t)
      }
      case 0 if hasNoParent =>
      case 0 => {
        sendToken(parentCh)
        val t = tokens.dequeue() //Parent is the last destination for token
        finishedTokens.enqueue(t)
        emptyParent()
      }
      case _ => {
        val randChIndex = Rand.rollFromZero(nonParentChsToSend.size)
        val ch = nonParentChsToSend.remove(randChIndex)
        sendToken(ch)
      }
    }
  }

  def initiate(): Unit = {
    val t = Token(id)
    tokens.enqueue(t)
    log.write("Initiator: No Parent", clock.stamp())
    nonParentChsToSend = ArrayBuffer(chs.filter(_ != parentCh): _*)
    val firstCh: Channel = Rand.pickItem(chs)
    val cmd = ProcessToken(t)
    val msg = Message(cmd, this, clock.stamp())
    log.write("Sending on " + firstCh, msg.ts)
    firstCh.recv(msg)
    val firstChIndex = nonParentChsToSend.indexOf(firstCh)
    nonParentChsToSend.remove(firstChIndex)
    initiated = true
  }

  private def hasNoParent: Boolean = parentCh == Channel.empty

  private def sendToken(ch: Channel) = {
    val pt = ProcessToken(tokens.last)
    val msg = Message(pt, this, clock.stamp())
    log.write("Sending on " + ch, msg.ts)
    ch.recv(msg)
  }

  private def processToken(t: Token, sender: Process): Unit = {
    if (tokens.isEmpty && parentCh == Channel.empty && finishedTokens.isEmpty) {
      parent = sender
      log.write("Parent: " + parent, clock.stamp())
      parentCh = chs.filter(_.hasTarget(sender))(0)
      nonParentChsToSend = ArrayBuffer(chs.filter(_ != parentCh): _*)
      tokens.enqueue(t)
    }
  }

  private def emptyParent(): Unit = {
    parentCh = Channel.empty
  }

  override def toString: String = "TNode" + id
}
