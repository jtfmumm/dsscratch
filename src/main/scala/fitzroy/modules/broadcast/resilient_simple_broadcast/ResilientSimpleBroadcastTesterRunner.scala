package fitzroy.modules.broadcast.resilient_simple_broadcast

import fitzroy.components._
import fitzroy.draw._
import fitzroy.model._
import fitzroy.modules._
import fitzroy.modules.broadcast._
import fitzroy.modules.test._
import fitzroy.topology._

object ResilientSimpleBroadcastTesterRunner {
  def runFor(nodeCount: Int, density: Double) = {
    val moduleBuilders = List(BroadcastTesterModule,
      ResilientSimpleBroadcastModule)

    val topologyBuilder = TopologyBuilder(nodeCount, density)
      .withModuleBuilders(moduleBuilders)

    val systemModel = SystemModel().withCrashRecovery(0.8, 100)

    ModuleTest("Resilient Simple Broadcast")
      .testFor(ModuleCodes.BROADCAST_TESTER)
      .terminateFor(Seq(ModuleCodes.RESILIENT_SIMPLE_BROADCAST))
      .withTopologyBuilder(topologyBuilder)
      .withSystemModel(systemModel)
      .withTimeout(100000)
      .run(5)
  }
}

