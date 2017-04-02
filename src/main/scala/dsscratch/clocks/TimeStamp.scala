package dsscratch.clocks


trait TimeStamp

case object EmptyTimeStamp extends TimeStamp {
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

case class VecStamp(vecMap: Map[Int, Int], id: Int = -1) extends TimeStamp {
  def inc(): VecStamp = {
    val incd = vecMap.updated(id, vecMap(id) + 1)
    VecStamp(incd, id)
  }
  def withId(id: Int): VecStamp = VecStamp(vecMap, id)
  def mergeWith(other: VecStamp): VecStamp = {
    // We can expect that both clocks have the same keys
    // If you want dynamic keys, use DynVec instead of Vec
    val merged = vecMap.keys.map(k => {
      if (vecMap(k) >= other.vecMap(k))
        k -> vecMap(k)
      else
        k -> other.vecMap(k)
    }).toMap
    VecStamp(merged, id)
  }
  def hasEntry(key: Int): Boolean = vecMap.get(key) match {
    case Some(_) => true
    case _ => false
  }

  def >(other: VecStamp): Boolean = vecMap.keys == other.vecMap.keys && vecMap.forall(pair => pair._2 > other.vecMap(pair._1))
  def >=(other: VecStamp): Boolean = vecMap.keys == other.vecMap.keys && vecMap.forall(pair => pair._2 >= other.vecMap(pair._1))
  def <(other: VecStamp): Boolean = vecMap.keys == other.vecMap.keys && vecMap.forall(pair => pair._2 < other.vecMap(pair._1))
  def <=(other: VecStamp): Boolean = vecMap.keys == other.vecMap.keys && vecMap.forall(pair => pair._2 <= other.vecMap(pair._1))

  def isCausallyRelatedTo(other: VecStamp): Boolean = this >= other || this <= other
  def isConcurrentWith(other: VecStamp): Boolean = !isCausallyRelatedTo(other)

  private def vectorToString: String = (for (x <- vecMap.toList.sortBy(_._1)) yield x._1 + ":" + x._2).mkString(",")
  override def toString: String = "<<" + id + "::[" + vectorToString + "]>>"
}

//Dynamic Vector Clock timestamp
case class DynVecStamp(vecMap: Map[Int, Int], id: Int = -1) extends TimeStamp {
  def inc(): DynVecStamp = {
    val incd = vecMap.updated(id, vecMap(id) + 1)
    DynVecStamp(incd, id)
  }
  def withId(id: Int): DynVecStamp = DynVecStamp(vecMap, id)

  //Allows new entries for nodes not yet encountered
  def mergeWith(other: DynVecStamp): DynVecStamp = {
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
    DynVecStamp(merged, id)
  }
  def hasEntry(key: Int): Boolean = vecMap.get(key) match {
    case Some(_) => true
    case _ => false
  }

  def >(other: DynVecStamp): Boolean = {
    (other.vecMap.keys.toSet subsetOf vecMap.keys.toSet) &&
      vecMap.forall(pair => pair._2 > other.vecMap.getOrElse(pair._1, -1))
  }
  def >=(other: DynVecStamp): Boolean = {
    (other.vecMap.keys.toSet subsetOf vecMap.keys.toSet) &&
      vecMap.forall(pair => pair._2 >= other.vecMap.getOrElse(pair._1, -1))
  }
  def <(other: DynVecStamp): Boolean = {
    (vecMap.keys.toSet subsetOf other.vecMap.keys.toSet) &&
      vecMap.forall(pair => pair._2 < other.vecMap(pair._1))
  }
  def <=(other: DynVecStamp): Boolean = {
    (vecMap.keys.toSet subsetOf other.vecMap.keys.toSet) &&
      vecMap.forall(pair => pair._2 <= other.vecMap(pair._1))
  }

  def isCausallyRelatedTo(other: DynVecStamp): Boolean = this >= other || this <= other
  def isConcurrentWith(other: DynVecStamp): Boolean = !isCausallyRelatedTo(other)

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
