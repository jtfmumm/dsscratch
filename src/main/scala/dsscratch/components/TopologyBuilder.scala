package dsscratch.components

import dsscratch.algos.nodes._
import dsscratch.components._

class TopologyBuilder(val nodeCount: Int, val density: Double) {
  assert(density >= 0 && density <= 1,
    "Topology density must be between 0 and 1")
  var moduleBuilders = Seq[NodeModuleBuilder]()
  var tConnected = (density == 1)

  def withModuleBuilders(ms: Seq[NodeModuleBuilder]): TopologyBuilder = {
    moduleBuilders = moduleBuilders ++ ms
    this
  }

  def totallyConnected(): TopologyBuilder = {
    tConnected = true
    this
  }

  def build(): Topology = {
    val initiator = Node()
    val nonInitiators = (2 to nodeCount).map(x => Node())
    val nodes = Seq(initiator) ++ nonInitiators
    val nodeIds = nodes.map(_.id).toSet
    for (mb <- moduleBuilders) {
      initiator.addModule(mb(initiator, nodeIds,
        isInitiator = true))
      nonInitiators.foreach((nd: Node) => nd.addModule(
        mb(nd, nodeIds)))
    }

    if (tConnected) {
      Topology.totallyConnected(nodes)
    } else {
      //No self connections
      val maxEdges = (nodeCount * (nodeCount - 1)) - nodeCount
      //Topology must be connected, so we need at least one path of n - 1 edges
      val possibleExtras = maxEdges - (nodeCount - 1)

      val extras = (possibleExtras * density).floor.toInt

      Topology.connectedWithKMoreEdges(extras, nodes)
    }
  }
}

object TopologyBuilder {
  def apply(nodeCount: Int, density: Double): TopologyBuilder = {
    new TopologyBuilder(nodeCount, density)
  }
}
