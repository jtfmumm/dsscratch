package dsscratch.modules.broadcast.echo

import dsscratch.components._
import dsscratch.draw._
import dsscratch.model._
import dsscratch.modules._
import dsscratch.modules.broadcast._
import dsscratch.modules.test._
import dsscratch.topology._

object EchoTesterRunner {
  def runFor(nodeCount: Int, density: Double) = {
    val moduleBuilders = List(BroadcastTesterModule, EchoModule)

    val topologyBuilder = TopologyBuilder(nodeCount, density)
      .withModuleBuilders(moduleBuilders)

    ModuleTest("Echo Algorithm")
      .testFor(ModuleCodes.BROADCAST_TESTER)
      .terminateFor(Seq(ModuleCodes.ECHO))
      .withTopologyBuilder(topologyBuilder)
      .withTimeout(100000)
      // .verbose()
      .run(5)
  }
}
