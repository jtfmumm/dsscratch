package dsscratch.runner

import dsscratch.components.Topology
import dsscratch.components.Steppable
import scala.util.Random

case class TopologyRunner[P <: Steppable](t: Topology[P], endCondition: () => Boolean) {
  val nodes = t.nodes
  val chs = t.chs
  val steppables: Seq[Steppable] = nodes ++ chs
  var moment = 0

  def run(): Unit = {
    while(!endCondition()) {
      moment = moment + 1
      step()
    }
  }

  def step(): Unit = {
    val shuffled = Random.shuffle(steppables)
    for (x <- shuffled) x.step()
  }
}
