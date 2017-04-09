package fitzroy.clocks

trait Clock {
  def stamp(): TimeStamp
  def compareAndUpdate(ts: TimeStamp): Unit
  def snapshot: Clock
}

case class EmptyClock() extends Clock {
  def stamp(): TimeStamp = TS()
  def compareAndUpdate(ts: TimeStamp): Unit = {}
  def snapshot: Clock = this
}
