package dsscratch.algos.tarry

import dsscratch.components._
import randomific.Rand
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer


//Tarry's algorithm
//Kicked off by one initiator
//1) A process never forwards the token through the same channel twice
//2) A process only forwards the token to its parent when there is no other option


case class Token(id: Int)

case class ProcessToken(t: Token, ch: Channel) extends Command


case class TNode(id: Int) extends Process {
  val chs = ArrayBuffer[Channel]()
  val tokens = Queue[Token]()
  val finishedTokens = Queue[Token]()

  var parentCh: Channel = Channel.empty
  var nonParentChsToSend = ArrayBuffer[Channel]()

  def recv(m: Message): Unit = {
    println(this + " is receiving from " + m.sender)
    m.cmd match {
      case ProcessToken(t, ch) => processToken(t, ch)
      case _ =>
    }
  }

  def step(): Unit = {
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

  def addChannel(ch: Channel): Unit = {
    if (!chs.contains(ch)) chs.append(ch)
  }

  def removeChannel(ch: Channel): Unit = {
    if (!chs.contains(ch)) return
    val i = chs.indexOf(ch)
    chs.remove(i)
  }

  def initiate(t: Token): Unit = {
    tokens.enqueue(t)
    nonParentChsToSend = ArrayBuffer(chs.filter(_ != parentCh): _*)
    println(this + " is initiating with " + t)
    val firstCh: Channel = Rand.pickItem(chs)
    val cmd = ProcessToken(t, firstCh)
    firstCh.recv(Message(cmd, this))
    val firstChIndex = nonParentChsToSend.indexOf(firstCh)
    nonParentChsToSend.remove(firstChIndex)
  }

  private def hasNoParent: Boolean = parentCh == Channel.empty

  private def sendToken(ch: Channel) = {
    val pt = ProcessToken(tokens.last, ch)
    ch.recv(Message(pt, this))
  }

  private def processToken(t: Token, ch: Channel): Unit = {
    println("Processing")
    if (tokens.isEmpty && parentCh == Channel.empty && finishedTokens.isEmpty) {
      parentCh = ch
      nonParentChsToSend = ArrayBuffer(chs.filter(_ != parentCh): _*)
      tokens.enqueue(t)
      println("I am " + this)
      println("My parent is " + parentCh)
    }
  }

  private def emptyChsToSend(): Unit = {
    nonParentChsToSend = ArrayBuffer[Channel]()
  }

  private def emptyParent(): Unit = {
    parentCh = Channel.empty
  }
}

object Tarry {
  def runFor(nodeCount: Int, density: Double) = {
    assert(density >= 0 && density <= 1)
    val nodes = (1 to nodeCount).map(x => TNode(x))
    println("Initial nodes: " + nodes)

    val maxEdges = (nodeCount * (nodeCount - 1)) - nodeCount //Rule out self connections
    val possibleExtras = maxEdges - (nodeCount - 1) //Topology must be connected, so we need at least one path of n - 1 edges

    val extras = (possibleExtras * density).floor.toInt

    val topology: Topology[TNode] = Topology.connectedWithKMoreEdges(extras, nodes)

    topology.nodes(0).initiate(Token(1))
    println("*******TOPOLOGY*********")
    println(topology.nodes)
    println(topology.chs)
    println("************************")
    while (topology.nodes.exists(nd => nd.finishedTokens.isEmpty)) {
      for (nd <- topology.nodes) nd.step()
      for (ch <- topology.chs) ch.deliverNext()
    }
  }
}