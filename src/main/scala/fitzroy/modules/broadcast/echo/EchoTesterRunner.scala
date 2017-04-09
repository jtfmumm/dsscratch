package fitzroy.modules.broadcast.echo

import fitzroy.components._
import fitzroy.draw._
import fitzroy.model._
import fitzroy.modules._
import fitzroy.modules.broadcast._
import fitzroy.modules.test._
import fitzroy.topology._

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
