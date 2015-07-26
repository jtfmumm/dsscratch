package dsscratch.runners

import scala.collection.mutable.ArrayBuffer
import dsscratch.components.Topology
import dsscratch.components.Steppable
import dsscratch.components.Channel
import dsscratch.components.Process
import scala.util.Random
import randomific.Rand

///////////////////////
//Configuration Options
//
//  crashOdds -> Int   //Odds some process will crash any moment
//  restartOdds -> Int  //Odds some crashed process will restart
//
///////////////////////
case class TopologyRunner(t: Topology,
                          endCondition: () => Boolean,
                          options: Map[String, Double] = Map[String, Double]()) {
  val config = setConfig(options)
  val nodes = t.nodes
  val chs = t.chs
  val failedNodes = ArrayBuffer[Process]()
  val partitionedChs = ArrayBuffer[Channel]()
  val steppables: Seq[Steppable] = nodes ++ chs
  var moment = 0

  def run(): Unit = {
    if (failuresArePossible) runWithFailures() else runWithoutFailures()
  }

  def runWithoutFailures(): Unit = {
    while(!endCondition()) {
      moment = moment + 1
      step()
    }
  }

  def runWithFailures(): Unit = {
    while(!endCondition()) {
      moment = moment + 1
      checkForRestart()
      checkForFailure()
      step()
    }
  }

  def checkForFailure(): Unit = {
    if (Rand.rolledByOdds(config("failureOdds"))) {
      val failedNode = Rand.pickItem(nodes)
      failedNode.fail()
      if (!failedNodes.contains(failedNode)) failedNodes.append(failedNode)
    }
  }

  def checkForRestart(): Unit = {
    if (failedNodes.size > 0 && Rand.rolledByOdds(config("restartOdds"))) {
      val idx = Rand.rollFromZero(failedNodes.size)
      val restartedNode = failedNodes.remove(idx)
      restartedNode.restart()
    }
  }

  def step(): Unit = {
    val shuffled = Random.shuffle(steppables)
    for (x <- shuffled) x.step()
  }

  private def failuresArePossible: Boolean = config("failureOdds") > 0

  private def setConfig(o: Map[String, Double]): Map[String, Double] = {
    Map(
      "failureOdds" -> o.getOrElse("failureOdds", 0.0),
      "restartOdds" -> o.getOrElse("restartOdds", 0.0)
    )
  }
}


