package dsscratch.util

import scala.collection.mutable.ArrayBuffer

case class Log {
  var data = ArrayBuffer[Any]()
  var curLine = 0

  def write(a: Any) = data.append(a)
  def readLine(): Any = {
    if (curLine == data.size) {
      "EOF"
    } else {
      val line = data(curLine)
      curLine = curLine + 1
      line
    }
  }
  def seek(n: Int): Unit = {
    if (n > data.size) return
    curLine = n
  }
  def readLines(): List[Any] = data.toList

  override def toString: String = {
    (for (l <- data) yield l.toString + "\n").mkString
  }
}
