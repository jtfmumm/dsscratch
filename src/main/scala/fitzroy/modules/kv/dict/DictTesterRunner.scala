package fitzroy.modules.kv.dict

import fitzroy.components._
import fitzroy.draw._
import fitzroy.model._
import fitzroy.modules._
import fitzroy.modules.consensus.two_phase_commit._
import fitzroy.modules.kv._
import fitzroy.modules.test._
import fitzroy.modules.broadcast.echo._
import fitzroy.topology._

object DictTesterRunner {
  def runFor(nodeCount: Int, density: Double) = {
    val moduleBuilders = List(KVTesterModule, DictModule, TwoPhaseCommitModule,
      EchoModule)

    val topology = TopologyBuilder(nodeCount, density)
      .withModuleBuilders(moduleBuilders)
      .build()

    def endCondition: Boolean = topology.nodes.forall({
      case nd: Node => nd.terminatedFor(ModuleCodes.DICT)
      case _ => true
    })

    TopologyRunner(topology, endCondition _, SystemModel()).run()

    //TRACE
    for (nd <- topology.nodes) {
      println(nd)
      println(nd.log)
    }
    //PARENTS
    println("//NETWORK")
    println(DotGraph.drawChs(topology.chs))
    //Spanning tree
    println("//TEST RESULTS")
    println("//All nodes should have a key of test and value of 1")
    println(Draw.asStrings(
        topology.nodes.map({
          case nd: Node => nd.resultFor(ModuleCodes.DICT)
          case _ => ""
        })
      )
    )
  }
}
