package dsscratch.algos.runners

import dsscratch.components._
import dsscratch.algos._
import dsscratch.algos.tarry._
import dsscratch.algos.nodes._
import dsscratch.model._
import dsscratch.runners.TopologyRunner
import dsscratch.draw.DotGraph

object TarryRunner {
  def runFor(nodeCount: Int, density: Double) = {
    val moduleBuilders = List(TarryModule)

    val topology = TopologyBuilder(nodeCount, density)
      .withModuleBuilders(moduleBuilders)
      .build()

    def endCondition: Boolean =
      topology.nodes.forall(_.terminatedFor(AlgoCodes.TARRY))

    TopologyRunner(topology, endCondition _, SystemModel()).run()

    //TRACE
    for (nd <- topology.nodes) {
      println(nd)
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
