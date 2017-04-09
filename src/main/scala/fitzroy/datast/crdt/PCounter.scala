package fitzroy.datast.crdt

import fitzroy.components._

// A simple PCounter implementation
class PCounter(val thisId: ProcessId, d: Map[ProcessId, Int]) {
  private val data: Map[ProcessId, Int] = d

  def inc(): PCounter = {
    new PCounter(thisId, data.updated(thisId, data(thisId) + 1))
  }

  def merge(other: PCounter): PCounter = {
    var newData = data
    for ((nodeId, count) <- other.data) {
      newData = newData.updated(nodeId, Math.max(newData(nodeId), count))
    }
    new PCounter(thisId, newData)
  }
}

object PCounter {
  def apply(thisId: ProcessId, nodeIds: Set[ProcessId]): PCounter = {
    var data = Map[ProcessId, Int]()
    for (n <- nodeIds) {
    if (!data.contains(n))
      data = data.updated(n, 0)
    }
    require(data.contains(thisId))
    new PCounter(thisId, data)
  }
}
