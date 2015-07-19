package dsscratch.clocks


trait TimeStamp

case class EmptyTimeStamp extends TimeStamp {
  override def toString: String = "<<Empty>>"
}

case class TS(value: Int, id: Int = -1) extends TimeStamp {
  def inc(): TS = TS(value + 1, id)
  def withId(id: Int): TS = TS(value, id)

  def >(other: TS) = if (value == other.value) id > other.id else value > other.value
  def >=(other: TS) = if (value == other.value) id >= other.id else value > other.value
  def <(other: TS) = if (value == other.value) id < other.id else value < other.value
  def <=(other: TS) = if (value == other.value) id <= other.id else value < other.value
  override def toString: String = "<<" + id + "::" + value + ">>"
}

case class Vec(vec: Map[Int, Int], id: Int = -1) extends TimeStamp {
  def inc(): Vec = {
    val incd = vec.updated(id, vec(id) + 1)
    Vec(incd, id)
  }
  def withId(id: Int): Vec = Vec(vec, id)
  def mergeWith(other: Vec): Vec = {
    val keys = vec.keys ++ other.vec.keys
    val merged = keys.map(k => {
      if (!this.hasEntry(k))
        k -> other.vec(k)
      else if (!other.hasEntry(k))
        k -> vec(k)
      else if (vec(k) >= other.vec(k))
        k -> vec(k)
      else
        k -> other.vec(k)
    }).toMap
    Vec(merged, id)
  }
  def hasEntry(key: Int): Boolean = vec.get(key) match {
    case Some(_) => true
    case _ => false
  }

  def >(other: Vec): Boolean = {
    (other.vec.keys.toSet subsetOf vec.keys.toSet) &&
      vec.forall(pair => pair._2 > other.vec.getOrElse(pair._1, -1))
  }
  def >=(other: Vec): Boolean = {
    (other.vec.keys.toSet subsetOf vec.keys.toSet) &&
      vec.forall(pair => pair._2 >= other.vec.getOrElse(pair._1, -1))
  }
  def <(other: Vec): Boolean = {
    (vec.keys.toSet subsetOf other.vec.keys.toSet) &&
      vec.forall(pair => pair._2 < other.vec(pair._1))
  }
  def <=(other: Vec): Boolean = {
    (vec.keys.toSet subsetOf other.vec.keys.toSet) &&
      vec.forall(pair => pair._2 <= other.vec(pair._1))
  }

  def isCausallyRelatedTo(other: Vec): Boolean = this >= other || this <= other
  def isConcurrentWith(other: Vec): Boolean = !isCausallyRelatedTo(other)

  private def vectorToString: String = (for (x <- vec.toList.sortBy(_._1)) yield x._1 + ":" + x._2).mkString(",")
  override def toString: String = "<<" + id + "::[" + vectorToString + "]>>"
}


case class Count(value: Int) extends TimeStamp {
  def inc(): Count = Count(value + 1)

  def >(other: Count) = value > other.value
  def >=(other: Count) = value >= other.value
  def <(other: Count) = value < other.value
  def <=(other: Count) = value <= other.value
  override def toString: String = "<<" + value + ">>"
}
