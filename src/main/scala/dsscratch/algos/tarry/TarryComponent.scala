package dsscratch.algos.tarry

import dsscratch.components._
import dsscratch.clocks._
import randomific.Rand
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer
import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.util.Log
import dsscratch.runner.TopologyRunner
import dsscratch.draw.DotGraph

//Tarry's algorithm
//Kicked off by one initiator
//1) A process never forwards the token through the same channel twice
//2) A process only forwards the token to its parent when there is no other option



trait TarryLocalState extends LocalState {
  var initiated: Boolean
  var initiator: Boolean
  var tokens: Queue[Token]
  var finishedTokens: Queue[Token]
  var parent: Process
  var parentCh: Channel
  var nonParentChsToSend: ArrayBuffer[Channel]
}

object TarryComponent {
  def apply(parentProcess: Process, isInitiator: Boolean = false): TarryComponent = {
    new TarryComponent(parentProcess, isInitiator)
  }
  def buildWith(parentProcess: Process, s: TarryLocalState): TarryComponent = {
    val newC = TarryComponent(parentProcess, s.initiator)

    newC.s.initiated = s.initiated
    newC.s.tokens = s.tokens.map(x => x).toQueue
    newC.s.finishedTokens = s.finishedTokens.map(x => x).toQueue
    newC.s.parent = s.parent
    newC.s.parentCh = s.parentCh
    newC.s.nonParentChsToSend = s.nonParentChsToSend.map(x => x)
    newC
  }
}

class TarryComponent(val parentProcess: Process, isInitiator: Boolean = false) extends NodeComponent {
  val algoCode: AlgoCode = AlgoCodes.TARRY
  val chs = parentProcess.outChs

  ////////////////////
  //LOCAL STATE
  private object s extends TarryLocalState {
    var initiated = false
    var initiator: Boolean = isInitiator
    var tokens = Queue[Token]()
    var finishedTokens = Queue[Token]()
    var parent: Process = EmptyProcess()
    var parentCh: Channel = Channel.empty
    var nonParentChsToSend = ArrayBuffer[Channel]()
  }
  ////////////////////

  def processMessage(m: Message): Unit = {
    m.cmd match {
      case ProcessToken(t) => processToken(t, m.sender)
      case _ =>
    }
  }

  def terminated = s.finishedTokens.nonEmpty

  def step(): Unit = {
    if (s.initiator && !s.initiated) initiate()
    if (s.tokens.isEmpty && s.finishedTokens.isEmpty) return
    s.nonParentChsToSend.size match {
      case 0 if hasNoParent && s.tokens.nonEmpty => {
        val t = s.tokens.dequeue()
        s.finishedTokens.enqueue(t)
      }
      case 0 if hasNoParent =>
      case 0 => {
        sendToken(s.parentCh)
        val t = s.tokens.dequeue() //Parent is the last destination for token
        s.finishedTokens.enqueue(t)
        emptyParent()
      }
      case _ => {
        val randChIndex = Rand.rollFromZero(s.nonParentChsToSend.size)
        val ch = s.nonParentChsToSend.remove(randChIndex)
        sendToken(ch)
      }
    }
  }

  def initiate(): Unit = {
    val t = Token(id)
    s.tokens.enqueue(t)
    log.write("[Tarry] Initiator: No Parent", clock.stamp())
    s.nonParentChsToSend = ArrayBuffer(chs.filter(_ != s.parentCh): _*)
    val firstCh: Channel = Rand.pickItem(chs)
    val cmd = ProcessToken(t)
    val msg = Message(cmd, parentProcess, clock.stamp())
    log.write("[Tarry] Sending on " + firstCh, msg.ts)
    firstCh.recv(msg)
    val firstChIndex = s.nonParentChsToSend.indexOf(firstCh)
    s.nonParentChsToSend.remove(firstChIndex)
    s.initiated = true
  }

  def snapshot: TarryComponent = TarryComponent.buildWith(parentProcess, s)

  def result = {
    if (s.parent != EmptyProcess())
      "  " + s.parent + " -> " + parentProcess + ";\n"
    else
      ""
  }

  private def hasNoParent: Boolean = s.parentCh == Channel.empty

  private def sendToken(ch: Channel) = {
    val pt = ProcessToken(s.tokens.last)
    val msg = Message(pt, parentProcess, clock.stamp())
    log.write("[Tarry] Sending on " + ch, msg.ts)
    ch.recv(msg)
  }

  private def processToken(t: Token, sender: Process): Unit = {
    if (s.tokens.isEmpty && s.parentCh == Channel.empty && s.finishedTokens.isEmpty) {
      s.parent = sender
      log.write("[Tarry] Parent: " + s.parent, clock.stamp())
      s.parentCh = chs.filter(_.hasTarget(sender))(0)
      s.nonParentChsToSend = ArrayBuffer(chs.filter(_ != s.parentCh): _*)
      s.tokens.enqueue(t)
    }
  }

  private def emptyParent(): Unit = {
    s.parentCh = Channel.empty
  }
}

object TarryRunner {
  def runFor(nodeCount: Int, density: Double) = {
    assert(density >= 0 && density <= 1)
    val initiator = Node(1)
    val nonInitiators = (2 to nodeCount).map(x => Node(x))
    initiator.addComponent(TarryComponent(initiator, isInitiator = true))
    nonInitiators.foreach((nd: Node) => nd.addComponent(TarryComponent(nd)))

    val nodes = Seq(initiator) ++ nonInitiators

    val maxEdges = (nodeCount * (nodeCount - 1)) - nodeCount //Rule out self connections
    val possibleExtras = maxEdges - (nodeCount - 1) //Topology must be connected, so we need at least one path of n - 1 edges

    val extras = (possibleExtras * density).floor.toInt

    val topology: Topology = Topology.connectedWithKMoreEdges(extras, nodes)

    def endCondition: Boolean = topology.nodes.forall({
      case nd: Node => nd.terminatedFor(AlgoCodes.TARRY)
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
          case nd: Node => nd.resultFor(AlgoCodes.TARRY)
          case _ => ""
        }).mkString + "}"
    )
  }
}

