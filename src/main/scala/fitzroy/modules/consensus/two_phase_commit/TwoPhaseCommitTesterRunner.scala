package fitzroy.modules.consensus.two_phase_commit

import fitzroy.components._
import fitzroy.draw._
import fitzroy.model._
import fitzroy.modules._
import fitzroy.modules.broadcast.echo._
import fitzroy.modules.consensus._
import fitzroy.modules.test._
import fitzroy.topology._

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
