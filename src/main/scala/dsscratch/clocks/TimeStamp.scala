package dsscratch.clocks

trait TimeStamp

case class EmptyTimeStamp extends TimeStamp {
  override def toString: String = "<<Empty>>"
}

case class TS(value: Int, id: Int = -1) extends TimeStamp {
  def inc(): TS = TS(value + 1, id)
  def withId(id: Int): TS = TS(value, id)

  def >(other: TS) = if (value == other.value) id > other.id else value > other.value
  def >=(other: TS) = if (value == other.value) id >= other.id else value > other.value
  def <(other: TS) = if (value == other.value) id < other.id else value < other.value
  def <=(other: TS) = if (value == other.value) id <= other.id else value < other.value
  override def toString: String = "<<" + id + "::" + value + ">>"
}

case class Count(value: Int) extends TimeStamp {
  def inc(): Count = Count(value + 1)

  def >(other: Count) = value > other.value
  def >=(other: Count) = value >= other.value
  def <(other: Count) = value < other.value
  def <=(other: Count) = value <= other.value
  override def toString: String = "<<" + value + ">>"
}
