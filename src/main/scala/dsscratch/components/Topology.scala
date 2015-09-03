package dsscratch.components

import randomific.Rand
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import dsscratch.util.Counter

case class Topology(nodes: Seq[Process], chs: ArrayBuffer[Channel])

object Topology {
  val chIdGen = Counter()
  val procIdGen = Counter()

  def createOneWayChannel(from: Process, to: Process, chs: ArrayBuffer[Channel]): Unit = {
    val newCh = TwoChannel(from, to, chIdGen.next())
    from.addOutChannel(newCh)
    to.addInChannel(newCh)
    chs.append(newCh)
  }

  def createUniqueOneWayChannel(p0: Process, p1: Process, chs: ArrayBuffer[Channel]): Unit = {
    if (chs.exists(_.containsPath(p0, p1))) return
    createOneWayChannel(p0, p1, chs)
  }

  def createTwoWayChannel(p0: Process, p1: Process, chs: ArrayBuffer[Channel]): Unit = {
    val newCh0 = TwoChannel(p0, p1, chIdGen.next())
    val newCh1 = TwoChannel(p1, p0, chIdGen.next())
    p0.addOutChannel(newCh0)
    p0.addInChannel(newCh1)
    p1.addOutChannel(newCh1)
    p1.addInChannel(newCh0)
    chs.append(newCh0)
    chs.append(newCh1)
  }

  def createUniqueTwoWayChannel(p0: Process, p1: Process, chs: ArrayBuffer[Channel]): Unit = {
    createUniqueOneWayChannel(p0, p1, chs)
    createUniqueOneWayChannel(p1, p0, chs)
  }

  def createMultiChannel(ps: Seq[Process], chs: ArrayBuffer[Channel]): Unit = {
    val newCh = MultiChannel(ps)
    for (p <- ps) {
      p.addOutChannel(newCh)
      p.addInChannel(newCh)
    }
    chs.append(newCh)
  }

  def star(center: Process, others: Seq[Process]): Topology = {
    val chs = ArrayBuffer[Channel]()
    for (nd <- others) createTwoWayChannel(center, nd, chs)

    Topology(center +: others, chs)
  }

  def bus(nodes: Seq[Process]): Topology = {
    val chs = ArrayBuffer[Channel]()
    createMultiChannel(nodes, chs)
    Topology(nodes, chs)
  }

  def line(nodes: Seq[Process]): Topology = {
    assert(nodes.size > 1)
    val chs = ArrayBuffer[Channel]()

    //create channels
    nodes.reduceLeft((last, next) => {
      createUniqueTwoWayChannel(last, next, chs)
      next
    })

    Topology(nodes, chs)
  }

  def ring(nodes: Seq[Process]): Topology = {
    assert(nodes.size > 1)
    val shuffled: Seq[Process] = Random.shuffle(nodes)

    val l: Topology = line(shuffled)
    val last: Process = l.nodes.reverse.head

    createTwoWayChannel(l.nodes.head, last, l.chs)
    Topology(l.nodes, l.chs)
  }

  def totallyConnected(nodes: Seq[Process]): Topology = {
    assert(nodes.size > 1)
    val chs = ArrayBuffer[Channel]()
    val shuffled: Seq[Process] = Random.shuffle(nodes)

    val pairs = cProductNoSelfPairs(shuffled)

    for (e: (Process, Process) <- pairs) {
      createUniqueOneWayChannel(e._1, e._2, chs)
    }

    Topology(shuffled, chs)
  }

  def connectedWithKMoreEdges(k: Int, nodes: Seq[Process]): Topology = {
    assert(nodes.size > 1 && k <= (((nodes.size * nodes.size) - (2 * nodes.size)) + 1)) //(n^2 - n) - (n - 1) = (n^2 - 2n) + 1 possible extra edges
    val shuffled: Seq[Process] = Random.shuffle(nodes)

    val l: Topology = line(shuffled) //Ensures connected path
    val chs = l.chs
    val pairs = orderedPairsNoSelf(l.nodes)

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

  private def orderedPairsNoSelf(s: Seq[Process]): Seq[(Process, Process)] = {
    (for (
      x: Process <- s;
      y: Process <- s
    ) yield {
      if (x > y) (y, x) else (x, y)
    }).filter(pair => pair._1 != pair._2).distinct
  }

}
