package fitzroy.modules.spanning_tree.tarry

import fitzroy.components._
import fitzroy.draw.DotGraph
import fitzroy.modules._
import fitzroy.modules.spanning_tree._
import fitzroy.model._
import fitzroy.topology._

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
