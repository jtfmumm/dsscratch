package dsscratch

import dsscratch.algos.runners._

object Run {
  def main(args: Array[String]) = {
//    TarryRunner.runFor(10, 0.5)

    // Buggy
    EchoTesterRunner.runFor(10, 0.5)
  }
}
