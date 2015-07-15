package dsscratch.components

import randomific.Rand
import scala.util.Random

class Topology {
  val nodes = List[Process]()
  val channels = List[Channel]()
}

object Topology {
  def createTwoChannel(p0: Process, p1: Process): Unit = {
    val newCh = TwoChannel(p0, p1)
    p0.addChannel(newCh)
    p1.addChannel(newCh)
  }

  def createMultiChannel(ps: Seq[Process]): Unit = {
    val newCh = MultiChannel(ps)
    for (p <- ps) p.addChannel(newCh)
  }

  def star(center: Process, others: Seq[Process]) = {
    for (nd <- others) createTwoChannel(center, nd)

    center +: others
  }

  def bus(nodes: Seq[Process]): Seq[Process] = {
    createMultiChannel(nodes)
    nodes
  }

  def line(nodes: Seq[Process]): Seq[Process] = {
    assert(nodes.size > 1)
    def loop(s: Seq[Process], acc: Seq[Process]): Seq[Process] = s match {
      case Nil => acc.reverse
      case h :: t => {
        createTwoChannel(h, t.head)
        h +: acc
      }
    }
    loop(nodes, Seq[Process]())
  }

  def ring(nodes: Seq[Process]): Seq[Process] = {
    assert(nodes.size > 1)
    val shuffled: Seq[Process] = Random.shuffle(nodes)

    val l: Seq[Process] = line(shuffled)
    val last: Process = l.reverse.head

    createTwoChannel(l.head, last)
    l //Now a ring
  }

  def totallyConnected(nodes: Seq[Process]): Seq[Process] = {
    assert(nodes.size > 1)
    val shuffled: Seq[Process] = Random.shuffle(nodes)

    val pairs = cProductNoSelfPairs(shuffled)

    for (e: (Process, Process) <- pairs) {
      createTwoChannel(e._1, e._2)
    }

    shuffled
  }

  def connectedWithKMoreEdges(nodes: Seq[Process], k: Int): Seq[Process] = {
    assert(nodes.size > 1 && k <= (((nodes.size * nodes.size) - (2 * nodes.size)) + 1)) //(n^2 - 2n) + 1 possible extra edges
    val shuffled: Seq[Process] = Random.shuffle(nodes)

    val l: Seq[Process] = line(shuffled) //Ensures connected path

    val pairs = cProductNoSelfPairs(l)

    val extraEdges = Rand.pickKItems(k, pairs)

    for (e: (Process, Process) <- extraEdges) {
      createTwoChannel(e._1, e._2)
    }

    l
  }

  private def cProductNoSelfPairs(s: Seq[Process]): Seq[(Process, Process)] = {
    s.flatMap(x =>
      s.tail.map(y => (x, y))
    )
  }
}
