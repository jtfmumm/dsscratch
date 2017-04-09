package dsscratch

import dsscratch.modules.broadcast.echo._
import dsscratch.modules.broadcast.simple_broadcast._
import dsscratch.modules.broadcast.resilient_simple_broadcast._
import dsscratch.modules.consensus.two_phase_commit._
import dsscratch.modules.kv.dict._
import dsscratch.modules.spanning_tree.tarry._

object Run {
  def main(args: Array[String]) = {
   // TODO: Create automatic test modules for these two
   // instead of requiring that one manually check results output
   // TarryRunner.runFor(10, 0.5)
   // DictTesterRunner.runFor(10, 0.125)

   EchoTesterRunner.runFor(100, 0.125)
   SimpleBroadcastTesterRunner.runFor(10, 0.125)
   ResilientSimpleBroadcastTesterRunner.runFor(10, 0.125)
   TwoPhaseCommitTesterRunner.runFor(20, 0.125)
  }
}
