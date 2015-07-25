package dsscratch.algos

import dsscratch.components._
import dsscratch.clocks._
import dsscratch.util.MutableDeque
import randomific.Rand
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer


class Node(val id: Int, val initiator: Boolean = false) extends Process {
  type Step = { def execute(): Unit }

  val nextSteps = MutableDeque[Step]()
  val clock = LamportClock(id)
  var initiated = false
  val chs = ArrayBuffer[Channel]()
  log.write(this + " log", clock.stamp())

  def recv(m: Message): Unit = {
    if (failed) return
    clock.compareAndUpdate(m.ts)
    log.write("Receiving " + m, clock.stamp())
    processCommand(m.cmd)
  }

  def step(): Unit = {
    if (failed) return
    if (initiator && !initiated) initiate()
    if (nextSteps.size > 0) nextSteps.pop().execute()
  }

  def processCommand(c: Command): Unit = {}

  def addChannel(ch: Channel): Unit = {
    if (!chs.contains(ch)) chs.append(ch)
  }

  def removeChannel(ch: Channel): Unit = {
    if (!chs.contains(ch)) return
    val i = chs.indexOf(ch)
    chs.remove(i)
  }

  def initiate(): Unit = {
  }

  override def toString: String = "Node" + id
}