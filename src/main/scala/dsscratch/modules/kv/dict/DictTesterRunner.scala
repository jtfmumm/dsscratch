package dsscratch.modules.kv.dict

import dsscratch.components._
import dsscratch.draw._
import dsscratch.model._
import dsscratch.modules._
import dsscratch.modules.consensus.two_phase_commit._
import dsscratch.modules.kv._
import dsscratch.modules.test._
import dsscratch.modules.broadcast.echo._
import dsscratch.topology._

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
