package dsscratch.clocks


trait TimeStamp

case class EmptyTimeStamp extends TimeStamp {
  override def toString: String = "<<Empty>>"
}

case class TS(value: Int = -1, id: Int = -1) extends TimeStamp {
  def inc(): TS = TS(value + 1, id)
  def withId(id: Int): TS = TS(value, id)

  def >(other: TS) = if (value == other.value) id > other.id else value > other.value
  def >=(other: TS) = if (value == other.value) id >= other.id else value > other.value
  def <(other: TS) = if (value == other.value) id < other.id else value < other.value
  def <=(other: TS) = if (value == other.value) id <= other.id else value < other.value
  override def toString: String = "<<" + id + "::" + value + ">>"
}

case class Vec(vecMap: Map[Int, Int], id: Int = -1) extends TimeStamp {
  def inc(): Vec = {
    val incd = vecMap.updated(id, vecMap(id) + 1)
    Vec(incd, id)
  }
  def withId(id: Int): Vec = Vec(vecMap, id)
  def mergeWith(other: Vec): Vec = {
    // We can expect that both clocks have the same keys
    // If you want dynamic keys, use DynVec instead of Vec
    val merged = vecMap.keys.map(k => {
      if (vecMap(k) >= other.vecMap(k))
        k -> vecMap(k)
      else
        k -> other.vecMap(k)
    }).toMap
    Vec(merged, id)
  }
  def hasEntry(key: Int): Boolean = vecMap.get(key) match {
    case Some(_) => true
    case _ => false
  }

  def >(other: Vec): Boolean = vecMap.keys == other.vecMap.keys && vecMap.forall(pair => pair._2 > other.vecMap(pair._1))
  def >=(other: Vec): Boolean = vecMap.keys == other.vecMap.keys && vecMap.forall(pair => pair._2 >= other.vecMap(pair._1))
  def <(other: Vec): Boolean = vecMap.keys == other.vecMap.keys && vecMap.forall(pair => pair._2 < other.vecMap(pair._1))
  def <=(other: Vec): Boolean = vecMap.keys == other.vecMap.keys && vecMap.forall(pair => pair._2 <= other.vecMap(pair._1))

  def isCausallyRelatedTo(other: Vec): Boolean = this >= other || this <= other
  def isConcurrentWith(other: Vec): Boolean = !isCausallyRelatedTo(other)

  private def vectorToString: String = (for (x <- vecMap.toList.sortBy(_._1)) yield x._1 + ":" + x._2).mkString(",")
  override def toString: String = "<<" + id + "::[" + vectorToString + "]>>"
}

//Dynamic Vector Clock timestamp
case class DynVec(vecMap: Map[Int, Int], id: Int = -1) extends TimeStamp {
  def inc(): DynVec = {
    val incd = vecMap.updated(id, vecMap(id) + 1)
    DynVec(incd, id)
  }
  def withId(id: Int): DynVec = DynVec(vecMap, id)

  //Allows new entries for nodes not yet encountered
  def mergeWith(other: DynVec): DynVec = {
    val keys = vecMap.keys ++ other.vecMap.keys
    val merged = keys.map(k => {
      if (!this.hasEntry(k))
        k -> other.vecMap(k)
      else if (!other.hasEntry(k))
        k -> vecMap(k)
      else if (vecMap(k) >= other.vecMap(k))
        k -> vecMap(k)
      else
        k -> other.vecMap(k)
    }).toMap
    DynVec(merged, id)
  }
  def hasEntry(key: Int): Boolean = vecMap.get(key) match {
    case Some(_) => true
    case _ => false
  }

  def >(other: DynVec): Boolean = {
    (other.vecMap.keys.toSet subsetOf vecMap.keys.toSet) &&
      vecMap.forall(pair => pair._2 > other.vecMap.getOrElse(pair._1, -1))
  }
  def >=(other: DynVec): Boolean = {
    (other.vecMap.keys.toSet subsetOf vecMap.keys.toSet) &&
      vecMap.forall(pair => pair._2 >= other.vecMap.getOrElse(pair._1, -1))
  }
  def <(other: DynVec): Boolean = {
    (vecMap.keys.toSet subsetOf other.vecMap.keys.toSet) &&
      vecMap.forall(pair => pair._2 < other.vecMap(pair._1))
  }
  def <=(other: DynVec): Boolean = {
    (vecMap.keys.toSet subsetOf other.vecMap.keys.toSet) &&
      vecMap.forall(pair => pair._2 <= other.vecMap(pair._1))
  }

  def isCausallyRelatedTo(other: DynVec): Boolean = this >= other || this <= other
  def isConcurrentWith(other: DynVec): Boolean = !isCausallyRelatedTo(other)

  private def vectorMapToString: String = (for (x <- vecMap.toList.sortBy(_._1)) yield x._1 + ":" + x._2).mkString(",")
  override def toString: String = "<<" + id + "::[" + vectorMapToString + "]>>"
}


case class Count(value: Int) extends TimeStamp {
  def inc(): Count = Count(value + 1)

  def >(other: Count) = value > other.value
  def >=(other: Count) = value >= other.value
  def <(other: Count) = value < other.value
  def <=(other: Count) = value <= other.value
  override def toString: String = "<<" + value + ">>"
}
