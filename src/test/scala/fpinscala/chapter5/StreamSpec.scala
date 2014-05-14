package fpinscala
package chapter5

import org.scalacheck.{Arbitrary, Gen}

class StreamSpec extends UnitSpec {

  implicit val positive = Arbitrary(Gen.choose(0, 20))

  it should "convert to list" in {
    forAll { (n: Int) =>
      val expected = (0 until n).toList
      Stream(expected:_*).toList shouldBe expected
    }
  }

  it should "take the first n elements" in {
    forAll { (n: Int, m: Int) =>
      val expected = (0 until n).toList
      Stream(expected:_*).take(m).toList shouldBe expected.take(m)
    }
  }

  it should "drop the first n elements" in {
    forAll { (n: Int, m: Int) =>
      val expected = (0 until n).toList
      Stream(expected:_*).drop(m).toList shouldBe expected.drop(m)
    }
  }

  it should "take while the predicate holds true" in {
    forAll { (n: Int) =>
      val expected = (0 until n).toList
      val p = (n: Int) => n < 5
      Stream(expected:_*).takeWhile(p).toList shouldBe expected.takeWhile(p)
    }
  }
}
