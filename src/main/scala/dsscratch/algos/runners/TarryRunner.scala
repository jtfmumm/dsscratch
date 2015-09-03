package dsscratch.algos.runners

import dsscratch.components._
import dsscratch.algos._
import dsscratch.algos.tarry._
import dsscratch.algos.nodes._
import dsscratch.runners.TopologyRunner
import dsscratch.draw.DotGraph

object TarryRunner {
  def runFor(nodeCount: Int, density: Double) = {
    assert(density >= 0 && density <= 1)
    val initiator = Node(1)
    val nonInitiators = (2 to nodeCount).map(x => Node(x))
    initiator.addComponent(TarryComponent(initiator, isInitiator = true))
    nonInitiators.foreach((nd: Node) => nd.addComponent(TarryComponent(nd)))

    val nodes = Seq(initiator) ++ nonInitiators

    val maxEdges = (nodeCount * (nodeCount - 1)) - nodeCount //No self connections
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
    println(DotGraph.drawChs(topology.chs))
    //Spanning tree
    println("//SPANNING TREE")
    println(DotGraph.drawStrings(
        topology.nodes.map({
          case nd: Node => nd.resultFor(AlgoCodes.TARRY)
          case _ => ""
        })
      )
    )
  }
}
