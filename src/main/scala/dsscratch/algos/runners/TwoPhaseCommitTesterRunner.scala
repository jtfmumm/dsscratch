package dsscratch.algos.runners

import dsscratch.algos._
import dsscratch.algos.broadcast._
import dsscratch.algos.consensus._
import dsscratch.algos.nodes._
import dsscratch.algos.test._
import dsscratch.components._
import dsscratch.draw._
import dsscratch.model._

object TwoPhaseCommitTesterRunner {
  def runFor(nodeCount: Int, density: Double) = {
    val moduleBuilders = List(CommitTesterModule, TwoPhaseCommitModule,
      EchoModule)

    val topology = TopologyBuilder(nodeCount, density)
      .withModuleBuilders(moduleBuilders)
      .build()

    def endCondition: Boolean = topology.nodes.forall({
      case nd: Node => nd.terminatedFor(AlgoCodes.TWO_PHASE_COMMIT)
      case _ => true
    })

    AlgoTest.test("Two Phase Commit", topology, endCondition _,
      AlgoCodes.COMMIT_TESTER, SystemModel())
  }
}
