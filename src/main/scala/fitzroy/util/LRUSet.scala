package fitzroy.util

import scala.collection.mutable.{Queue, Set => mSet}

// Grow only set that only stores the MAX latest members
// Defaults to containing only the last 250 members added.
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

  def isEmpty: Boolean = data.isEmpty
  def nonEmpty: Boolean = data.nonEmpty

  override def toString: String = "mLRUSet(" + lru.mkString(", ") + ")"
}

object mLRUSet {
  def apply[A](els: A*): mLRUSet[A] = {
    val s = new mLRUSet[A]()
    for (el <- els) s += el
    s
  }
  def withMax[A](mx: Int)(els: A*): mLRUSet[A] = {
    val s = new mLRUSet[A](max = mx)
    for (el <- els) s += el
    s
  }
}
