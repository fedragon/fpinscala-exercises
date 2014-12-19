package fpinscala
package chapter6

import org.scalacheck.{Arbitrary, Gen}

class RNGSpec extends UnitSpec {

  implicit val positiveInt = Arbitrary(Gen.choose(1, 10))
  implicit val positiveLong = Arbitrary(Gen.choose(0L, 10L))

  it should "generate a random non negative integer" in {
    forAll { (seed: Long) =>
      val (value, _) = RNG.nonNegativeInt(SimpleRNG(seed))
      value should be >= 0
      value should be <= Int.MaxValue
    }
  }

  it should "generate a random double between 0 (included) and 1 (excluded)" in {
    forAll { (seed: Long) =>
      {
        val (value, _) = RNG.double(SimpleRNG(seed))
        value should be >= 0d
        value should be < 1d
      }

      {
        val (value, _) = RNG.doubleWithMap(SimpleRNG(seed))
        value should be >= 0d
        value should be < 1d
      }
    }
  }

  it should "generate a random pair (Int, Double)" in {
    forAll { (seed: Long) =>
      {
        val ((i, d), _) = RNG.intDouble(SimpleRNG(seed))
        i should be >= 0
        i should be <= Int.MaxValue

        d should be >= 0d
        d should be < 1d
      }

      {
        val ((i, d), _) = RNG.intDoubleWithMap2(SimpleRNG(seed))
        i should be >= 0
        i should be <= Int.MaxValue

        d should be >= 0d
        d should be < 1d
      }
    }
  }

  it should "generate a random pair (Double, Int)" in {
    forAll { (seed: Long) =>
      val ((dValue, iValue), _) = RNG.doubleInt(SimpleRNG(seed))
      iValue should be >= 0
      iValue should be <= Int.MaxValue

      dValue should be >= 0d
      dValue should be < 1d
    }
  }

  it should "generate 3 random doubles" in {
    forAll { (seed: Long) =>
      val ((d1, d2, d3), _) = RNG.double3(SimpleRNG(seed))

      d1 should be >= 0d
      d1 should be < 1d
      d2 should be >= 0d
      d2 should be < 1d
      d3 should be >= 0d
      d3 should be < 1d
    }
  }

  it should "generate a list of random integers" in {
    forAll { (count: Int, seed: Long) =>
      {
        val (is, _) = RNG.ints(count)(SimpleRNG(seed))

        is.foreach { i =>
          i should be >= Int.MinValue
          i should be <= Int.MaxValue
        }
      }

      {
        val (is, _) = RNG.intsWithSequence(count)(SimpleRNG(seed))

        is.foreach { i =>
          i should be >= Int.MinValue
          i should be <= Int.MaxValue
        }
      }
    }
  }

  it should "generate a random non negative integer less than n" in {
    forAll { (n: Int, seed: Long) =>
      val (value, _) = RNG.nonNegativeLessThan(n)(SimpleRNG(seed))
      value should be >= 0
      value should be <= n
    }
  }

  it should "combine two randoms" in {
    forAll { (s1: Long, s2: Long) =>

    }
  }
}
