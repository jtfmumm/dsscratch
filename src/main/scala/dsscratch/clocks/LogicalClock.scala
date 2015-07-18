package dsscratch.clocks


case class LogicalClock {
  var tick = Count(0)

  def stamp(): TimeStamp = {
    tick = tick.inc()
    tick
  }

  def compareAndUpdate(other: TimeStamp): Unit = other match {
    case c @ Count(_) => if (tick < c) tick = c.inc() else tick = tick.inc()
    case _ => tick = tick.inc()
  }
}
