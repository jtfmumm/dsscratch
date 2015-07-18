package dsscratch.util

import scala.collection.mutable.ArrayBuffer
import dsscratch.clocks.{TimeStamp, EmptyTimeStamp}

case class Log {
  var data = ArrayBuffer[String]()
  var curLine = 0

  def write(a: Any, ts: TimeStamp = EmptyTimeStamp()) = data.append(ts + ": " + a.toString)
  def readLine(): String = {
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
  def readLines(): List[String] = data.toList

  override def toString: String = {
    (for (l <- data) yield l.toString + "\n").mkString
  }
}