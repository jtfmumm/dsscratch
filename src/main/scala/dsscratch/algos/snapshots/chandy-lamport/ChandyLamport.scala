package dsscratch.algos.tarry

import dsscratch.components._
import dsscratch.clocks._
import randomific.Rand
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer
import dsscratch.util.Log
import dsscratch.runner.TopologyRunner
import dsscratch.draw.DotGraph

//Chandy-Lamport snapshot algorithm
//Kicked off by any initiator (here we choose one)
//1) The initiator takes a local snapshot and sends control messages through all channels.
//2) When a process receives a control message for the first time, it takes a local
//   snapshot and sends control messages through all channels.
//3) A process p records the state of an incoming channel c by taking a snapshot of all incoming
//   messages M on c from the time p first received a control message to the time that a
//   control message is received over c (these can coincide, in which case M is empty)
//4) A process terminates when it has received control messages over all channels and has sent
//   control messages over all channels.

case class TakeSnapshot(snapshotId: Int) extends Command

case class CLNode(id: Int, initiator: Boolean = false) extends Process {
  val clock = LamportClock(id)
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

  def addChannel(ch: Channel): Unit = {
    if (!chs.contains(ch)) chs.append(ch)
  }

  def removeChannel(ch: Channel): Unit = {
    if (!chs.contains(ch)) return
    val i = chs.indexOf(ch)
    chs.remove(i)
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

object ChandyLamport {
  def runFor(nodeCount: Int, density: Double) = {

    assert(density >= 0 && density <= 1)
    val initiator = TNode(1, initiator = true)
    val nonInitiators = (2 to nodeCount).map(x => TNode(x))
    val nodes = Seq(initiator) ++ nonInitiators

    val maxEdges = (nodeCount * (nodeCount - 1)) - nodeCount //Rule out self connections
    val possibleExtras = maxEdges - (nodeCount - 1) //Topology must be connected, so we need at least one path of n - 1 edges

    val extras = (possibleExtras * density).floor.toInt

    val topology: Topology = Topology.connectedWithKMoreEdges(extras, nodes)

    def endCondition: Boolean = topology.nodes.forall({
      case nd: TNode => nd.finishedTokens.nonEmpty
      case _ => true
    })

    TopologyRunner(topology, endCondition _).run()

    //TRACE
    for (nd <- topology.nodes) {
      println("Next")
      println(nd.log)
    }
    //PARENTS
    println("*****PARENTS******")
    for (nd <- topology.nodes) {
      println(nd.log.readLine(0))
      println(nd.log.firstMatchFor("Parent"))
      println("----")
    }
    println("//NETWORK")
    println(DotGraph.draw(topology.chs))
    //Spanning tree
    println("//SPANNING TREE")
    println(
      "digraph H {\n" +
        topology.nodes.map({
          case nd: TNode => {
            if (nd.parent != EmptyProcess())
              "  " + nd.parent + " -> " + nd + ";\n"
            else
              ""
          }
          case _ => ""
        }).mkString + "}"
    )
  }
}