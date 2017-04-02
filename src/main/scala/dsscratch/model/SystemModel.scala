package dsscratch.model


class SystemModel(val delayOdds: Double = 0.0,
                  val maxDelay: Int = 0,
                  val nodeFailureOdds: Double = 0.0,
                  val nodeRestartOdds: Double = 0.0,
                  val nodeCrashStopOdds: Double = 0.0,
                  val partitionOdds: Double = 0.0,
                  val healPartitionOdds: Double = 0.0) {

  def withNetworkPartitions(pOdds: Double = 0.1, hOdds: Double = 0.1):
    SystemModel = {
    new SystemModel(delayOdds, maxDelay, nodeFailureOdds, nodeRestartOdds,
      nodeCrashStopOdds, pOdds, hOdds)
  }

  def withMessageDelays(dOdds: Double = 0.05, maxD: Int = 5): SystemModel = {
    new SystemModel(dOdds, maxD, nodeFailureOdds, nodeRestartOdds,
      nodeCrashStopOdds, partitionOdds, healPartitionOdds)
  }

  def withCrashRestart(fOdds: Double = 0.1, rOdds: Double = 0.1): SystemModel =
  {
    new SystemModel(delayOdds, maxDelay, fOdds, rOdds,  nodeCrashStopOdds,
      partitionOdds, healPartitionOdds)
  }

  def withCrashStop(cOdds: Double = 0.01): SystemModel = {
    new SystemModel(delayOdds, maxDelay, nodeFailureOdds, nodeRestartOdds,
      cOdds, partitionOdds, healPartitionOdds)
  }
}

object SystemModel {
  def apply(): SystemModel = {
    new SystemModel()
  }
}

