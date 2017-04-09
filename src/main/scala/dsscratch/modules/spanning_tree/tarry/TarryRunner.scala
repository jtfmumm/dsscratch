package dsscratch.modules.spanning_tree.tarry

import dsscratch.components._
import dsscratch.draw.DotGraph
import dsscratch.modules._
import dsscratch.modules.spanning_tree._
import dsscratch.model._
import dsscratch.topology._

object TarryRunner {
  def runFor(nodeCount: Int, density: Double) = {
    val moduleBuilders = List(TarryModule)

    val topology = TopologyBuilder(nodeCount, density)
      .withModuleBuilders(moduleBuilders)
      .build()

    def endCondition: Boolean =
      topology.nodes.forall(_.terminatedFor(ModuleCodes.TARRY))

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
          case nd: Node => nd.resultFor(ModuleCodes.TARRY)
          case _ => ""
        })
      )
    )
  }
}
