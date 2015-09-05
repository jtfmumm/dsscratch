package dsscratch.util

import scala.collection.mutable.{Queue, Set => mSet}

// Grow only set that only stores the MAX latest members
class mLRUSet[A](val data: mSet[A] = mSet[A](), val max: Int = 250) {
  val lru = Queue[A]()
  for (a <- data) lru.enqueue(a)

  def +=(a: A): mLRUSet[A] = {
    if (data.contains(a)) return this
    data += a
    lru.enqueue(a)
    if (lru.size > max) {
      val rmv = lru.dequeue()
      data -= rmv
    }
    this
  }

  def contains(a: A): Boolean = data.contains(a)
}
