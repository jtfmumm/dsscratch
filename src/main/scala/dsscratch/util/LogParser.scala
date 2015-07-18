package dsscratch.util

object LogParser {
  def <(l0: String, l1: String): Boolean = getTime(l0) < getTime(l1)
  def <=(l0: String, l1: String): Boolean = getTime(l0) <= getTime(l1)
  def >(l0: String, l1: String): Boolean = getTime(l0) > getTime(l1)
  def >=(l0: String, l1: String): Boolean = getTime(l0) >= getTime(l1)

  def findLineWith(l: String)

  private def getTime(l: String): Int = l.dropWhile(_ == '<').takeWhile(_.isDigit).toInt
}
