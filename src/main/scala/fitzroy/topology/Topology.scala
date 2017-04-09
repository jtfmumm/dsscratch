package fitzroy.topology

import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import fitzroy.client_server._
import fitzroy.components._
import fitzroy.draw._
import fitzroy.util.Counter
import fitzroy.util.Rand

case class Topology(nodes: Seq[Node], chs: ArrayBuffer[Channel],
  var clients: Set[Client] = Set[Client]())
{
  def withClients(clientLogic: ClientLogic): Topology = {
    nodes.foreach(nd => {
      val client = Client(clientLogic)
      client.connect(nd)
      clients = clients + client
    })
    this
  }
}

object Topology {
  val chIdGen = Counter()
  val procIdGen = Counter()

  def createOneWayChannel(from: Node, to: Node,
    chs: ArrayBuffer[Channel]): Unit = {
    val newCh = TwoChannel(from, to, chIdGen.next())
    from.addOutChannel(newCh)
    to.addInChannel(newCh)
    chs.append(newCh)
  }

  def createUniqueOneWayChannel(p0: Node, p1: Node,
    chs: ArrayBuffer[Channel]): Unit = {
    if (chs.exists(_.containsPath(p0, p1))) return
    createOneWayChannel(p0, p1, chs)
  }

  def createTwoWayChannel(p0: Node, p1: Node,
    chs: ArrayBuffer[Channel]): Unit = {
    val newCh0 = TwoChannel(p0, p1, chIdGen.next())
    val newCh1 = TwoChannel(p1, p0, chIdGen.next())
    p0.addOutChannel(newCh0)
    p0.addInChannel(newCh1)
    p1.addOutChannel(newCh1)
    p1.addInChannel(newCh0)
    chs.append(newCh0)
    chs.append(newCh1)
  }

  def createUniqueTwoWayChannel(p0: Node, p1: Node,
    chs: ArrayBuffer[Channel]): Unit = {
    createUniqueOneWayChannel(p0, p1, chs)
    createUniqueOneWayChannel(p1, p0, chs)
  }

  def createMultiChannel(ps: Seq[Node], chs: ArrayBuffer[Channel]): Unit = {
    val newCh = MultiChannel(ps.toSet, chIdGen.next())
    for (p <- ps) {
      p.addOutChannel(newCh)
      p.addInChannel(newCh)
    }
    chs.append(newCh)
  }

  def star(center: Node, others: Seq[Node]): Topology = {
    val chs = ArrayBuffer[Channel]()
    for (nd <- others) createTwoWayChannel(center, nd, chs)

    Topology(center +: others, chs)
  }

  def bus(nodes: Seq[Node]): Topology = {
    val chs = ArrayBuffer[Channel]()
    createMultiChannel(nodes, chs)
    Topology(nodes, chs)
  }

  def line(nodes: Seq[Node]): Topology = {
    assert(nodes.size > 1)
    val chs = ArrayBuffer[Channel]()

    //create channels
    nodes.reduceLeft((last, next) => {
      createUniqueTwoWayChannel(last, next, chs)
      next
    })

    Topology(nodes, chs)
  }

  def ring(nodes: Seq[Node]): Topology = {
    assert(nodes.size > 1)
    val shuffled: Seq[Node] = Random.shuffle(nodes)

    val l: Topology = line(shuffled)
    val last: Node = l.nodes.reverse.head

    createTwoWayChannel(l.nodes.head, last, l.chs)
    Topology(l.nodes, l.chs)
  }

  def totallyConnected(nodes: Seq[Node]): Topology = {
    assert(nodes.size > 1)
    val chs = ArrayBuffer[Channel]()
    val shuffled: Seq[Node] = Random.shuffle(nodes)

    val pairs = cProductNoSelfPairs(shuffled)

    for (e: (Node, Node) <- pairs) {
      createUniqueOneWayChannel(e._1, e._2, chs)
    }

    Topology(shuffled, chs)
  }

  def connectedWithKMoreEdges(k: Int, nodes: Seq[Node]): Topology = {
    //(n^2 - n) - (n - 1) = (n^2 - 2n) + 1 possible extra edges
    assert(nodes.size > 1 && k <= (((nodes.size * nodes.size) -
      (2 * nodes.size)) + 1))

    val shuffled: Seq[Node] = Random.shuffle(nodes)

    val l: Topology = line(shuffled) //Ensures connected path
    val chs = l.chs
    val pairs = orderedPairsNoSelf(l.nodes)

    val extraEdges = Rand.pickKItems(k, pairs)

    for (e: (Node, Node) <- extraEdges) {
      createUniqueTwoWayChannel(e._1, e._2, chs)
    }

    Topology(l.nodes, chs) //Some edges may have been chosen twice
  }

  def dotgraph(t: Topology): String = {
    DotGraph.drawChs(t.chs)
  }

  def trace(t: Topology): String = {
    var tr = ""
    for (nd <- t.nodes) {
      tr += "NODE" + nd.id + "\n"
      tr += nd.log + "\n"
    }
    tr
  }

  private def cProductNoSelfPairs(s: Seq[Node]): Seq[(Node, Node)] = {
    (for (
      x: Node <- s;
      y: Node <- s
    ) yield (x, y)).filter(pair => pair._1 != pair._2)
  }

  private def orderedPairsNoSelf(s: Seq[Node]): Seq[(Node, Node)] = {
    (for (
      x: Node <- s;
      y: Node <- s
    ) yield {
      if (x > y) (y, x) else (x, y)
    }).filter(pair => pair._1 != pair._2).distinct
  }

}
