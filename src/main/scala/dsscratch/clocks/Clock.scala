package dsscratch.clocks

trait Clock {
  def stamp(): TimeStamp
  def compareAndUpdate(ts: TimeStamp): Unit
}