package dsscratch.util


case class Counter(initial: Int = 0) {
  var latest = initial

  def next(): Int = {
    latest = latest + 1
    latest
  }
}
