package dsscratch.algos.nodes

import dsscratch.components.{Steppable, Message, Process}
import dsscratch.algos._

trait NodeComponent extends Steppable {
  val parentProcess: Process
  val algoCode: AlgoCode

  val id = parentProcess.id
  val log = parentProcess.log
  val clock = parentProcess.clock

  def processMessage(m: Message): Unit
  def snapshot(): NodeComponent
  def terminated: Boolean
  def result: String
}
