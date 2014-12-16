package fpinscala
package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (value, state) = rng.nextInt

    if(value == Int.MinValue)
      (0, state)
    else
      (Math.abs(value), state)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (value, state) = nonNegativeInt(rng)

    (value.toDouble / Int.MaxValue, state)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, state) = nonNegativeInt(rng)
    val (d, state2) = double(rng)

    ((i, d), state2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (id, state) = intDouble(rng)

    (id.swap, state)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, state) = double(rng)
    val (d2, state2) = double(state)
    val (d3, state3) = double(state2)

    ((d1, d2, d3), state3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 until count).foldLeft((Nil: List[Int], rng)) { (acc, _) =>
      val (is, state) = acc
      val (i, newState) = nonNegativeInt(state)

      (i :: is, newState)
    }
  }
}
