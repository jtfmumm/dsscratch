package dsscratch.dict

import scala.collection.mutable.{Map => mMap}

class Dictionary(var d: mMap[String, Int] = mMap[String, Int]()) {
  def get(k: String) = d.getOrElse(k, -1)

  def put(k: String, v: Int) = {
    assert(v > 0, "Values must be positive integers!") // This is a very positive dictionary
    d(k) = v
  }

  def delete(k: String) = d.remove(k)

  override def clone(): Dictionary = new Dictionary(d.map(x => x))
  override def toString: String = {
    "{{ \n" +
    d.map(pair => pair._1 + ": " + pair._2).mkString("\n") +
    "\n}}"
  }
}

object Dictionary {
  def apply(): Dictionary = new Dictionary()
  def apply(d: mMap[String, Int]): Dictionary = new Dictionary(d)
}
