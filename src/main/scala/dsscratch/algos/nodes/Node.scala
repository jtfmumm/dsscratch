package dsscratch.algos.nodes

import dsscratch.algos._
import dsscratch.components._
import dsscratch.clocks._
import dsscratch.algos.snapshots.Snapshot
import dsscratch.util.MutableDeque
import randomific.Rand
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer

trait NodeLocalState extends LocalState {}

class Node(val id: Int, clk: Clock, val initiator: Boolean = false) extends Process {
  var components = ArrayBuffer[NodeComponent]()
  var snapshots = ArrayBuffer[Snapshot]()
  override val clock = clk
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
  }

  def processMessage(m: Message): Unit = components.foreach(_.processMessage(m))

  def addComponent(c: NodeComponent) = if (!components.exists(_.algoCode == c.algoCode)) components.append(c)

  def initiate(): Unit = {
  }

  override def takeSnapshot(snapId: TimeStamp) = {
    val snap = new Node(id, clock.snapshot, initiator)
    snap.components = components.map(_.snapshot)
    snap.initiated = initiated
    snap.snapshots = snapshots
    snapshots.append(Snapshot(snapId, snap))
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
  def apply(id: Int, initiator: Boolean = false): Node = new Node(id, LamportClock(id), initiator = initiator)
}