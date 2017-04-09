package dsscratch.util

import scala.util.Random

object Rand {
  val rnd = new Random

  def randInt(low: Int, high: Int): Int = {
    assert(high >= low)
    low + rnd.nextInt((high - low) + 1)
  }

  def roll(sides: Int): Int = {
    assert(sides > 0)
    randInt(1, sides)
  }

  def nRolls(n: Int, sides: Int): Int = {
    assert(n > 0)
    (1 to n).foldLeft(0)((acc, x) => acc + roll(sides))
  }

  def rollFromZero(sides: Int): Int = {
    assert(sides > 0)
    randInt(0, sides - 1)
  }

  def flip(): Int = rollFromZero(2)

  def rolledByOdds(odds: Double): Boolean = {
    assert(odds >= 0 && odds <= 1)
    Math.random < odds
  }

  def pickItem[A](s: Seq[A]): A = {
    assert(s.nonEmpty)
    val pick = rollFromZero(s.size)
    s(pick)
  }

  def kRandIntsBetween(k: Int, low: Int, high: Int): Seq[Int] = {
    assert(k >= 0 && k <= (high - low))

    var ints = Seq[Int]()
    while (ints.size < k) {
      ints = randInt(low, high) +: ints
    }
    ints

    // def loop(n: Int, acc: Seq[Int]): Seq[Int] = n match {
    //   case 0 => acc
    //   case _ => {
    //     val next: Seq[Int] = (randInt(low, high) +: acc).distinct
    //     if (next.size > acc.size) loop(n - 1, next) else loop(n, acc)
    //   }
    // }
    // loop(k, Seq[Int]())
  }

  def pickKItems[A](k: Int, s: Seq[A]): Seq[A] = {
    assert(k >= 0 && k <= s.size)
    val indices = kRandIntsBetween(k, 0, s.size - 1)
    indices.map(i => s(i))
  }
}
