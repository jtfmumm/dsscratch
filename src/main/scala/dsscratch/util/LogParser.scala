package dsscratch.util

object LogParser {
  def <(l0: String, l1: String): Boolean = {
    if (getTime(l0) == getTime(l1))
      getId(l0) < getId(l1)
    else
      getTime(l0) < getTime(l1)
  }
  def <=(l0: String, l1: String): Boolean = {
    if (getTime(l0) == getTime(l1))
      getId(l0) <= getId(l1)
    else
      getTime(l0) < getTime(l1)
  }
  def >(l0: String, l1: String): Boolean = {
    if (getTime(l0) == getTime(l1))
      getId(l0) > getId(l1)
    else
      getTime(l0) > getTime(l1)
  }
  def >=(l0: String, l1: String): Boolean = {
    if (getTime(l0) == getTime(l1))
      getId(l0) >= getId(l1)
    else
      getTime(l0) > getTime(l1)
  }

  private def getTime(l: String): Int = l.dropWhile(_ == '<').takeWhile(_.isDigit).toInt
  private def getId(l: String): Int = l.dropWhile(x => x == '<' || x.isDigit || x == '>' || x == '[').takeWhile(_.isDigit).toInt

}
