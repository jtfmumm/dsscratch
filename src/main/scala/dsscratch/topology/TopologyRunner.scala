package dsscratch.topology

import scala.collection.mutable.ArrayBuffer
import dsscratch.components.Channel
import dsscratch.components.Node
import dsscratch.components.Process
import dsscratch.components.Steppable
import dsscratch.model.SystemModel
import scala.util.Random
import dsscratch.util.Rand

import scala.collection.mutable.{Set => mSet}


case class TopologyRunner(t: Topology,
                          endCondition: () => Boolean,
                          val systemModel: SystemModel,
                          timeout: Int = 0)
{
  val nodes = t.nodes
  val chs = t.chs
  val clients = t.clients
  val failedNodes = mSet[Node]()
  val partitionedChs = mSet[Channel]()
  val steppables: Seq[Steppable] = nodes ++ chs ++ clients
  var moment = 0

  def run(): Unit = {
    while(!endCondition() && !timedOut) {
      moment = moment + 1
      checkCrashRecovery()
      checkCrashStop()
      checkPartition()
      checkForDelay()
      step()
    }
  }

  def step(): Unit = {
    val shuffled = Random.shuffle(steppables)
    for (x <- shuffled) x.step()
  }

  def timedOut: Boolean = {
    if (timeout > 0)
      moment > timeout
    else
      false
  }

  def checkCrashRecovery(): Unit = {
    nodes.foreach(nd => {
      if (failedNodes.contains(nd)) {
        nd.recoveryStep()
        if (nd.timeToRecovery == 0) {
          failedNodes -= nd
          nd.restart()
        }
      } else if (Rand.rolledByOdds(systemModel.nodeCrashRecoveryOdds)) {
        nd.fail(Rand.roll(systemModel.maxCrashDowntime))
        failedNodes += nd
      }
    })
  }

  def checkCrashStop(): Unit = {
    // Check if we should trigger a crash-stop node crash
    nodes.foreach(nd => {
      if (Rand.rolledByOdds(systemModel.nodeCrashStopOdds)) {
        // Since this isn't added to failedNodes, it will never be restarted
        nd.fail()
      }
    })
  }

  def checkPartition(): Unit = {
    chs.foreach(ch => {
      if (partitionedChs.contains(ch)) {
        ch.recoveryStep()
        if (ch.timeToRecovery == 0) {
          partitionedChs -= ch
          ch.restart()
        }
      } else if (Rand.rolledByOdds(systemModel.partitionOdds)) {
        ch.fail(Rand.roll(systemModel.maxPartitionDowntime))
        partitionedChs += ch
      }
    })
  }

  def checkForDelay(): Unit = {
    if (Rand.rolledByOdds(systemModel.delayOdds)) {
      val delayedCh = Rand.pickItem(chs)
      val delay = Rand.rollFromZero(systemModel.maxDelay)
      delayedCh.delay(delay)
    }
  }
}
