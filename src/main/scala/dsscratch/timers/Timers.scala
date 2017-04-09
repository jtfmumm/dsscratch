package dsscratch.timers

import scala.collection.mutable.{Set => mSet}


class Timers {
  private val timers = mSet[Timer]()

  def addTimer(t: Timer) = timers += t
  def removeTimer(t: Timer) = timers -= t

  def tick() = {
    timers.foreach(_.tick)
    timers.filter(!_.isActive()).foreach(t => timers -= t)
  }
}

object Timers {
  def apply(): Timers = new Timers
}

class Timer(val timeout: Int, f: () => Unit, repeat: Boolean = false) {
  require(timeout > 0, "Timer: timeout must be greater than 0")
  private var counter = timeout

  def tick() = {
    if (isActive()) {
      counter -= 1
      if (counter == 0) {
        f()
        if (repeat) counter = timeout
      }
    }
  }

  def isActive(): Boolean = (counter > 0) || repeat
}

object Timer {
  def apply(timeout: Int, f: () => Unit, repeat: Boolean = false) = {
    new Timer(timeout, f, repeat)
  }
}
