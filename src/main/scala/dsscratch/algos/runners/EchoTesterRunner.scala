package dsscratch.algos.runners

import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.algos.test._
import dsscratch.algos.broadcast._
import dsscratch.components._
import dsscratch.draw.DotGraph
import dsscratch.runners.TopologyRunner

object EchoTesterRunner {
  def runFor(nodeCount: Int, density: Double) = {
    assert(density >= 0 && density <= 1)
    val initiator = Node(1)
    val nonInitiators = (2 to nodeCount).map(x => Node(x))
    initiator.addComponent(EchoTesterComponent(initiator, isInitiator = true))
    nonInitiators.foreach((nd: Node) => nd.addComponent(EchoTesterComponent(nd)))
    initiator.addComponent(EchoComponent(initiator, isInitiator = true))
    nonInitiators.foreach((nd: Node) => nd.addComponent(EchoComponent(nd)))
    initiator.addComponent(SimpleTestComponent(initiator))
    nonInitiators.foreach((nd: Node) => nd.addComponent(SimpleTestComponent(nd)))

    val nodes = Seq(initiator) ++ nonInitiators

    val maxEdges = (nodeCount * (nodeCount - 1)) - nodeCount //No self connections
    val possibleExtras = maxEdges - (nodeCount - 1) //Topology must be connected, so we need at least one path of n - 1 edges

    val extras = (possibleExtras * density).floor.toInt

    val topology: Topology = Topology.connectedWithKMoreEdges(extras, nodes)

    def endCondition: Boolean = topology.nodes.forall({
      case nd: Node => nd.terminatedFor(AlgoCodes.ECHO)
      case _ => true
    })

    TopologyRunner(topology, endCondition _).run()

    //TRACE
    for (nd <- topology.nodes) {
      println("Next")
      println(nd.log)
    }
    //PARENTS
    println("//NETWORK")
    println(DotGraph.drawChs(topology.chs))
    //Spanning tree
    println("//TEST RESULTS")
    println(DotGraph.drawStrings(
        topology.nodes.map({
          case nd: Node => nd.resultFor(AlgoCodes.SIMPLE_TEST)
          case _ => ""
        })
      )
    )
  }
}
