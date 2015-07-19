package dsscratch.runner

import dsscratch.components.Topology
import scala.util.Random

class TopologyRunner[P](t: Topology[P]) {
  def step(): Unit = {
    val nodes = Random.shuffle(t.nodes)
    val chs = Random.shuffle(t.chs)
  }
}
