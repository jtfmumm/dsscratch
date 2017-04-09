package dsscratch.modules.broadcast.resilient_simple_broadcast

import dsscratch.components._
import dsscratch.draw._
import dsscratch.model._
import dsscratch.modules._
import dsscratch.modules.broadcast._
import dsscratch.modules.test._
import dsscratch.topology._

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
