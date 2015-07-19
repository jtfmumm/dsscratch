package dsscratch.components

import randomific.Rand
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import dsscratch.util.Counter

case class Topology[P](nodes: Seq[P], chs: ArrayBuffer[Channel])

object Topology {
  val chIdGen = Counter()
  val procIdGen = Counter()

  def createOneWayChannel(from: Process, to: Process, chs: ArrayBuffer[Channel]): Unit = {
    val newCh = TwoChannel(from, to, chIdGen.next())
    from.addChannel(newCh)
    chs.append(newCh)
  }

  def createUniqueOneWayChannel(p0: Process, p1: Process, chs: ArrayBuffer[Channel]): Unit = {
    if (chs.exists(_.containsPath(p0, p1))) return
    createOneWayChannel(p0, p1, chs)
  }

  def createTwoWayChannel(p0: Process, p1: Process, chs: ArrayBuffer[Channel]): Unit = {
    val newCh0 = TwoChannel(p0, p1, chIdGen.next())
    val newCh1 = TwoChannel(p1, p0, chIdGen.next())
    p0.addChannel(newCh0)
    p1.addChannel(newCh1)
    chs.append(newCh0)
    chs.append(newCh1)
  }

  def createUniqueTwoWayChannel(p0: Process, p1: Process, chs: ArrayBuffer[Channel]): Unit = {
    createUniqueOneWayChannel(p0, p1, chs)
    createUniqueOneWayChannel(p1, p0, chs)
  }

  def createMultiChannel(ps: Seq[Process], chs: ArrayBuffer[Channel]): Unit = {
    val newCh = MultiChannel(ps)
    for (p <- ps) p.addChannel(newCh)
    chs.append(newCh)
  }

  def star[P <: Process](center: P, others: Seq[P]): Topology[P] = {
    val chs = ArrayBuffer[Channel]()
    for (nd <- others) createTwoWayChannel(center, nd, chs)

    Topology(center +: others, chs)
  }

  def bus[P <: Process](nodes: Seq[P]): Topology[P] = {
    val chs = ArrayBuffer[Channel]()
    createMultiChannel(nodes, chs)
    Topology(nodes, chs)
  }

  def line[P <: Process](nodes: Seq[P]): Topology[P] = {
    assert(nodes.size > 1)
    val chs = ArrayBuffer[Channel]()
    def loop(s: Seq[P], acc: Seq[P]): Seq[P] = s match {
      case a if s.isEmpty => acc.reverse
      case b if b.tail.isEmpty => (b.head +: acc).reverse
      case c => {
        createUniqueTwoWayChannel(s.head, s.tail.head, chs)
        loop(s.tail, s.head +: acc)
      }
    }
    val finalNodes = loop(nodes, Seq[P]())
    Topology(finalNodes, chs)
  }

  def ring[P <: Process](nodes: Seq[P]): Topology[P] = {
    assert(nodes.size > 1)
    val shuffled: Seq[P] = Random.shuffle(nodes)

    val l: Topology[P] = line(shuffled)
    val chs = l.chs
    val last: P = l.nodes.reverse.head

    createTwoWayChannel(l.nodes.head, last, chs)
    Topology(l.nodes, chs)
  }

  def totallyConnected[P <: Process](nodes: Seq[P]): Topology[P] = {
    assert(nodes.size > 1)
    val chs = ArrayBuffer[Channel]()
    val shuffled: Seq[P] = Random.shuffle(nodes)

    val pairs = cProductNoSelfPairs(shuffled)

    for (e: (Process, Process) <- pairs) {
      createUniqueOneWayChannel(e._1, e._2, chs)
    }

    Topology(shuffled, chs)
  }

  def connectedWithKMoreEdges[P <: Process](k: Int, nodes: Seq[P]): Topology[P] = {
    assert(nodes.size > 1 && k <= (((nodes.size * nodes.size) - (2 * nodes.size)) + 1)) //(n^2 - n) - (n - 1) = (n^2 - 2n) + 1 possible extra edges
    val shuffled: Seq[P] = Random.shuffle(nodes)

    val l: Topology[P] = line(shuffled) //Ensures connected path
    val chs = l.chs
    val pairs = unorderedPairsNoSelf(l.nodes)

    val extraEdges = Rand.pickKItems(k, pairs)

    for (e: (Process, Process) <- extraEdges) {
      createUniqueTwoWayChannel(e._1, e._2, chs)
    }

    Topology(l.nodes, chs) //Some edges may have been chosen twice
  }

  private def cProductNoSelfPairs(s: Seq[Process]): Seq[(Process, Process)] = {
    (for (
      x: Process <- s;
      y: Process <- s
    ) yield (x, y)).filter(pair => pair._1 != pair._2)
  }

  private def unorderedPairsNoSelf(s: Seq[Process]): Seq[(Process, Process)] = {
    (for (
      x: Process <- s;
      y: Process <- s
    ) yield {
      if (x > y) (y, x) else (x, y)
    }).filter(pair => pair._1 != pair._2).distinct
  }

}
