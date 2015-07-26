package dsscratch.algos.nodes

import dsscratch.algos._
import dsscratch.components._
import dsscratch.clocks._
import dsscratch.util.MutableDeque
import randomific.Rand
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer


class Node(val id: Int, val initiator: Boolean = false) extends Process {
//  type Step = { def execute(): Unit }

//  val nextSteps = MutableDeque[Step]()
  val components = ArrayBuffer[NodeComponent]()
  val clock = LamportClock(id)
  var initiated = false
  log.write(this + " log", clock.stamp())

  def recv(m: Message): Unit = {
    if (failed) return
    clock.compareAndUpdate(m.ts)
    log.write("Receiving " + m, clock.stamp())
    processMessage(m)
  }

  def step(): Unit = {
    if (failed) return
    if (initiator && !initiated) initiate()
    components.foreach(_.step())
//    if (nextSteps.size > 0) nextSteps.pop().execute()
  }

  def processMessage(m: Message): Unit = components.foreach(_.processMessage(m))

  def addComponent(c: NodeComponent) = if (!components.exists(_.algoCode == c.algoCode)) components.append(c)

  def initiate(): Unit = {
  }

  def terminatedFor(algoCode: AlgoCode): Boolean = {
    if (!components.exists(_.algoCode == algoCode)) return false

    components.filter(_.algoCode == algoCode)(0).terminated
  }

  def resultFor(algoCode: AlgoCode): String = {
    if (!components.exists(_.algoCode == algoCode)) return ""

    components.filter(_.algoCode == algoCode)(0).result
  }

  override def toString: String = "Node" + id
}

object Node {
  def apply(id: Int, initiator: Boolean = false): Node = new Node(id, initiator)
}