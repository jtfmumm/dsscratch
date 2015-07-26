package dsscratch.clocks

trait Clock {
  def stamp(): TimeStamp
  def compareAndUpdate(ts: TimeStamp): Unit
}

case class EmptyClock() extends Clock {
  def stamp(): TimeStamp = TS()
  def compareAndUpdate(ts: TimeStamp): Unit = {}
}