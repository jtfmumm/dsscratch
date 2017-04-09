package fitzroy.util


case class Counter(var latest: Int = 0) {
  def next(): Int = {
    latest = latest + 1
    latest
  }
}
