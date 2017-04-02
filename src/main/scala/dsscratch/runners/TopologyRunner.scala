package dsscratch.runners

import scala.collection.mutable.ArrayBuffer
import dsscratch.algos.nodes._
import dsscratch.components.Channel
import dsscratch.components.Node
import dsscratch.components.Process
import dsscratch.components.Steppable
import dsscratch.components.Topology
import dsscratch.model.SystemModel
import scala.util.Random
import dsscratch.util.Rand


case class TopologyRunner(t: Topology,
                          endCondition: () => Boolean,
                          val systemModel: SystemModel,
                          timeout: Int = 0)
{
  val nodes = t.nodes
  val chs = t.chs
  val failedNodes = ArrayBuffer[Node]()
  val partitionedChs = ArrayBuffer[Channel]()
  val steppables: Seq[Steppable] = nodes ++ chs
  var moment = 0

  def run(): Unit = {
    while(!endCondition() && !timedOut) {
      moment = moment + 1
      checkForRestart()
      checkForFailure()
      checkForCrash()
      checkForPartitionHeal()
      checkForPartition()
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

  def checkForFailure(): Unit = {
    if (Rand.rolledByOdds(systemModel.nodeFailureOdds)) {
      val failedNode = Rand.pickItem(nodes)
      failedNode.fail()
      if (!failedNodes.contains(failedNode)) failedNodes.append(failedNode)
    }
  }

  def checkForRestart(): Unit = {
    if (failedNodes.nonEmpty &&
      Rand.rolledByOdds(systemModel.nodeRestartOdds))
    {
      val idx = Rand.rollFromZero(failedNodes.size)
      val restartedNode = failedNodes.remove(idx)
      restartedNode.restart()
    }
  }

  def checkForCrash(): Unit = {
    if (Rand.rolledByOdds(systemModel.nodeCrashStopOdds)) {
      val failedNode = Rand.pickItem(nodes)
      // Since this isn't added to failedNodes, it will never be restarted
      failedNode.fail()
    }
  }

  def checkForPartition(): Unit = {
    if (Rand.rolledByOdds(systemModel.partitionOdds)) {
      val partitionedCh = Rand.pickItem(chs)
      partitionedCh.fail()
      if (!partitionedChs.contains(partitionedCh))
        partitionedChs.append(partitionedCh)
    }
  }

  def checkForPartitionHeal(): Unit = {
    if (partitionedChs.nonEmpty &&
      Rand.rolledByOdds(systemModel.healPartitionOdds))
    {
      val idx = Rand.rollFromZero(partitionedChs.size)
      val healedCh = partitionedChs.remove(idx)
      healedCh.restart()
    }
  }

  def checkForDelay(): Unit = {
    if (Rand.rolledByOdds(systemModel.delayOdds)) {
      val delayedCh = Rand.pickItem(chs)
      val delay = Rand.rollFromZero(systemModel.maxDelay)
      delayedCh.delay(delay)
    }
  }
}
