package dsscratch.util

import scala.collection.mutable.ArrayBuffer
import dsscratch.clocks.{TimeStamp, EmptyTimeStamp}

import scala.util.matching.Regex


case class Log(data: ArrayBuffer[String] = ArrayBuffer[String]()) {
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

  def grep(re: Regex): Log = {
    val filtered = data.filter(l => {
      re.findFirstMatchIn(l) match {
        case Some(_) => true
        case _ => false
      }
    })
    Log(filtered)
  }

  def grep(s: String): Log = {
    grep(s.r)
  }

  def firstMatchFor(re: Regex): String = {
    grep(re).readLine()
  }

  def firstMatchFor(s: String): String = {
    grep(s).readLine()
  }

  override def toString: String = {
    (for (l <- data) yield l.toString + "\n").mkString
  }
}
