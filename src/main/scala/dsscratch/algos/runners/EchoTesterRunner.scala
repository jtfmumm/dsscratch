package dsscratch.algos.runners

import dsscratch.algos._
import dsscratch.algos.nodes._
import dsscratch.algos.test._
import dsscratch.algos.broadcast._
import dsscratch.components._
import dsscratch.draw._
import dsscratch.model._

object EchoTesterRunner {
  def runFor(nodeCount: Int, density: Double) = {
    val moduleBuilders = List(BroadcastTesterModule, EchoModule)

    val topology = TopologyBuilder(nodeCount, density)
      .withModuleBuilders(moduleBuilders)
      .build()

    // The echo algorithm
    def endCondition: Boolean =
      topology.nodes.forall(_.terminatedFor(AlgoCodes.ECHO))

    AlgoTest.test("Echo Algorithm", topology, endCondition _,
      AlgoCodes.BROADCAST_TESTER, SystemModel())
  }
}
