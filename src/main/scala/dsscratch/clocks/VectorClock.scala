package dsscratch.clocks


class VectorClock(val id: Int, val vec: Map[Int, Int]) {
  var tick = Vec(vec, id)

  def stamp(): TimeStamp = {
    tick = tick.inc()
    tick
  }

  def compareAndUpdate(other: TimeStamp): Unit = other match {
    case v @ Vec(_, _) => {
      if (tick <= v) tick = v.inc().withId(id)
      else if (tick >= v) tick = tick.inc()
      else tick = tick.mergeWith(v)
    }
    case _ => tick = tick.inc()
  }
}

object VectorClock {
  def apply(id: Int): VectorClock = new VectorClock(id, Map(id -> 0))
  def apply(id: Int, vec: Map[Int, Int]): VectorClock = new VectorClock(id, vec)
}
