package fitzroy.modules.broadcast.simple_broadcast

import fitzroy.components._
import fitzroy.draw._
import fitzroy.model._
import fitzroy.modules._
import fitzroy.modules.broadcast._
import fitzroy.modules.test._
import fitzroy.topology._

object SimpleBroadcastTesterRunner {
  def runFor(nodeCount: Int, density: Double) = {
    val moduleBuilders = List(BroadcastTesterModule, SimpleBroadcastModule)

    val topologyBuilder = TopologyBuilder(nodeCount, density)
      .withModuleBuilders(moduleBuilders)

    // If you run with this failure model, this test will fail. However,
    // the resilient version (ResilientSimpleBroadcast) will pass.
    // val systemModel = SystemModel().withCrashRecovery(0.8, 10)

    val systemModel = SystemModel()

    ModuleTest("Simple Broadcast")
      .testFor(ModuleCodes.BROADCAST_TESTER)
      .terminateFor(Seq(ModuleCodes.SIMPLE_BROADCAST))
      .withTopologyBuilder(topologyBuilder)
      .withSystemModel(systemModel)
      .withTimeout(100000)
      .run(5)

  }
}

