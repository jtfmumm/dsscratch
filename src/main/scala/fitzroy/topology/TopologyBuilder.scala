package fitzroy.topology

import fitzroy.modules._
import fitzroy.components._

class TopologyBuilder(var nodeCount: Int, var density: Double) {
  assert(density >= 0 && density <= 1,
    "Topology density must be between 0 and 1")
  var moduleBuilders = Seq[ModuleBuilder]()
  var tConnected = (density == 1)

  def withModuleBuilders(ms: Seq[ModuleBuilder]): TopologyBuilder = {
    moduleBuilders = moduleBuilders ++ ms
    this
  }

  def withNodeCount(count: Int): TopologyBuilder = {
    nodeCount = count
    this
  }

  def withEdgeDensity(d: Double): TopologyBuilder = {
    density = d
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
      val initModule = mb(NodeModuleParent(initiator), nodeIds,
        isInitiator = true)
      initiator.addModule(initModule)
      nonInitiators.foreach((nd: Node) => {
        val nextModule = mb(NodeModuleParent(nd), nodeIds)
        nd.addModule(nextModule)
      })
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
