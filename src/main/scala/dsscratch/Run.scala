package dsscratch

import dsscratch.algos.runners._

object Run {
  def main(args: Array[String]) = {
//    TarryRunner.runFor(10, 0.5)

//    EchoTesterRunner.runFor(10, 0.125)
//    SimpleBroadcastTesterRunner.runFor(10, 0.125)
//    TwoPhaseCommitTesterRunner.runFor(10, 0.125)
    DictTesterRunner.runFor(10, 0.125)
  }
}
