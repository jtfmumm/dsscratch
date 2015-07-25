package dsscratch.util

case class FDeque[A](leading: List[A], trailing: List[A]) {
  private def cleaned: FDeque[A] = if (leading.nonEmpty) this else FDeque(trailing.reverse, List.empty[A])
  def isEmpty(): Boolean = leading.isEmpty && trailing.isEmpty
  def prepend(a: A): FDeque[A] = {
    val clean = cleaned
    FDeque(a::clean.leading, clean.trailing)
  }
  def append(a: A): FDeque[A] = {
    val clean = cleaned
    FDeque(clean.leading, a::clean.trailing)
  }
  def pop(): (A, FDeque[A]) = {
    val clean = cleaned
    clean.leading match {
      case h::t => (h, FDeque(clean.leading.tail, clean.trailing))
      case _ => throw new RuntimeException("Empty deque has no first element!")
    }
  }
  def shift(): (A, FDeque[A]) = {
    val clean = cleaned
    clean.trailing match {
      case h::t => (h, FDeque(clean.leading, clean.trailing.tail))
      case _ if clean.leading.nonEmpty => {
        val rev = clean.leading.reverse
        val last = rev.head
        (last, FDeque(rev.tail.reverse, clean.trailing))
      }
      case _ => throw new RuntimeException("Empty deque has no last element!")
    }
  }
  def peekFirst(): (A, FDeque[A]) = {
    val clean = cleaned
    clean.leading match {
      case h::t => (h, FDeque(clean.leading, clean.trailing))
      case _ => throw new RuntimeException("Empty deque has no first element!")
    }
  }
  def peekLast(): (A, FDeque[A]) = {
    val clean = cleaned
    clean.trailing match {
      case h::t => (h, FDeque(clean.leading, clean.trailing))
      case _ => throw new RuntimeException("Empty deque has no last element!")
    }
  }
  def tail(): FDeque[A] = {
    val clean = cleaned
    FDeque(clean.leading.tail, clean.trailing)
  }
}
object FDeque {
  def empty[B]: FDeque[B] = FDeque(List.empty[B], List.empty[B])
}

class MutableDeque[A](l: List[A], t: List[A]) {
  var leading = l
  var trailing = t
  private def clean(): Unit = if (leading.isEmpty) {
    leading = trailing.reverse
    trailing = List.empty[A]
  }
  def isEmpty(): Boolean = leading.isEmpty && trailing.isEmpty
  def prepend(a: A): MutableDeque[A] = {
    clean()
    leading = a::leading
    this
  }
  def append(a: A): MutableDeque[A] = {
    clean()
    trailing = a::trailing
    this
  }
  def pop(): A = {
    clean()
    leading match {
      case h::t => {
        leading = leading.tail
        h
      }
      case _ => throw new RuntimeException("Empty deque has no first element!")
    }
  }
  def shift(): A = {
    clean()
    trailing match {
      case h::t => {
        trailing = trailing.tail
        h
      }
      case _ if leading.nonEmpty => {
        val rev = leading.reverse
        val last = rev.head
        leading = rev.tail.reverse
        last
      }
      case _ => throw new RuntimeException("Empty deque has no last element!")
    }
  }
  def peekFirst(): A = {
    clean()
    leading match {
      case h::t => h
      case _ => throw new RuntimeException("Empty deque has no first element!")
    }
  }
  def peekLast(): A = {
    clean()
    trailing match {
      case h::t => h
      case _ => throw new RuntimeException("Empty deque has no last element!")
    }
  }
  def tail(): MutableDeque[A] = {
    clean()
    new MutableDeque(leading.tail, trailing)
  }

  override def toString: String = "Leading: " + leading + ", " + "Trailing: " + trailing
}
object MutableDeque {
  def apply[B](l: List[B], t: List[B]) = new MutableDeque[B](l, t)
  def empty[B](): MutableDeque[B] = MutableDeque(List.empty[B], List.empty[B])
}