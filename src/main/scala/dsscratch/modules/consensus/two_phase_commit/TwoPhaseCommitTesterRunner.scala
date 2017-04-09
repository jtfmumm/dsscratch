package dsscratch.modules.consensus.two_phase_commit

import dsscratch.components._
import dsscratch.draw._
import dsscratch.model._
import dsscratch.modules._
import dsscratch.modules.broadcast.echo._
import dsscratch.modules.consensus._
import dsscratch.modules.test._
import dsscratch.topology._

object TwoPhaseCommitTesterRunner {
  def runFor(nodeCount: Int, density: Double) = {
    val moduleBuilders = List(CommitTesterModule, TwoPhaseCommitModule,
      EchoModule)

    val topologyBuilder = TopologyBuilder(nodeCount, density)
      .withModuleBuilders(moduleBuilders)

    ModuleTest("Two Phase Commit")
      .testFor(ModuleCodes.COMMIT_TESTER)
      .terminateFor(Seq(ModuleCodes.TWO_PHASE_COMMIT))
      .withTopologyBuilder(topologyBuilder)
      .withTimeout(100000)
      .run(5)

  }
}
