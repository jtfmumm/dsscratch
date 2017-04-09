package dsscratch.model


class SystemModel(val delayOdds: Double = 0.0,
                  val maxDelay: Int = 0,
                  val nodeCrashRecoveryOdds: Double = 0.0,
                  val maxCrashDowntime: Int = 0,
                  val nodeCrashStopOdds: Double = 0.0,
                  val partitionOdds: Double = 0.0,
                  val maxPartitionDowntime: Int = 0) {

  def withNetworkPartitions(pOdds: Double = 0.1, maxDowntime: Int = 20):
    SystemModel = {
    new SystemModel(delayOdds, maxDelay, nodeCrashRecoveryOdds,
      maxCrashDowntime, nodeCrashStopOdds, pOdds, maxDowntime)
  }

  def withMessageDelays(dOdds: Double = 0.05, maxD: Int = 5): SystemModel = {
    new SystemModel(dOdds, maxD, nodeCrashRecoveryOdds, maxCrashDowntime,
      nodeCrashStopOdds, partitionOdds, maxPartitionDowntime)
  }

  def withCrashRecovery(cOdds: Double = 0.1, maxDowntime: Int = 20):
    SystemModel = {
    new SystemModel(delayOdds, maxDelay, cOdds, maxDowntime, nodeCrashStopOdds,
      partitionOdds, maxPartitionDowntime)
  }

  def withCrashStop(cOdds: Double = 0.01): SystemModel = {
    new SystemModel(delayOdds, maxDelay, nodeCrashRecoveryOdds,
      maxCrashDowntime, cOdds, partitionOdds, maxPartitionDowntime)
  }
}

object SystemModel {
  def apply(): SystemModel = {
    new SystemModel()
  }
}

