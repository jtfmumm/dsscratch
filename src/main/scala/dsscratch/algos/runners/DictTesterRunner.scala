package dsscratch.algos.runners

import dsscratch.algos._
import dsscratch.algos.consensus.TwoPhaseCommitModule
import dsscratch.algos.nodes._
import dsscratch.algos.dict._
import dsscratch.algos.test._
import dsscratch.algos.broadcast._
import dsscratch.components._
import dsscratch.draw._
import dsscratch.model._
import dsscratch.runners.TopologyRunner

object DictTesterRunner {
  def runFor(nodeCount: Int, density: Double) = {
    val moduleBuilders = List(KVTesterModule, DictModule, TwoPhaseCommitModule,
      EchoModule)

    val topology = TopologyBuilder(nodeCount, density)
      .withModuleBuilders(moduleBuilders)
      .build()

    def endCondition: Boolean = topology.nodes.forall({
      case nd: Node => nd.terminatedFor(AlgoCodes.DICT)
      case _ => true
    })

    TopologyRunner(topology, endCondition _, SystemModel()).run()

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
    println("//All nodes should have a key of test and value of 1")
    println(Draw.asStrings(
        topology.nodes.map({
          case nd: Node => nd.resultFor(AlgoCodes.DICT)
          case _ => ""
        })
      )
    )
  }
}
