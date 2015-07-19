package dsscratch.clocks


case class LogicalClock(id: Int) {
  var tick = TS(0, id)

  def stamp(): TimeStamp = {
    tick = tick.inc()
    tick
  }

  def compareAndUpdate(other: TimeStamp): Unit = other match {
    case c @ TS(_, _) => if (tick < c) tick = c.inc().withId(id) else tick = tick.inc()
    case _ => tick = tick.inc()
  }
}
