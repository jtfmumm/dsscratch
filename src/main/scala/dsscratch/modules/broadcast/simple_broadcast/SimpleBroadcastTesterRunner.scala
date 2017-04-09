package dsscratch.modules.broadcast.simple_broadcast

import dsscratch.components._
import dsscratch.draw._
import dsscratch.model._
import dsscratch.modules._
import dsscratch.modules.broadcast._
import dsscratch.modules.test._
import dsscratch.topology._

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

