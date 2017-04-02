package dsscratch.algos.runners

import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.algos.test._
import dsscratch.algos.broadcast._
import dsscratch.components._
import dsscratch.draw._
import dsscratch.model._

object SimpleBroadcastTesterRunner {
  def runFor(nodeCount: Int, density: Double) = {
    val builders = List(BroadcastTesterModule, SimpleBroadcastModule)

    val topology = TopologyBuilder(nodeCount, density)
      .withModuleBuilders(builders)
      .build()

    def endCondition: Boolean =
      topology.nodes.forall(_.terminatedFor(AlgoCodes.SIMPLE_BROADCAST))

    AlgoTest.test("Simple Broadcast", topology, endCondition _,
      AlgoCodes.BROADCAST_TESTER, SystemModel())
  }
}

