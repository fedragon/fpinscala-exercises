package fpinscala
package chapter5

import org.scalacheck.{Arbitrary, Gen}

class StreamSpec extends UnitSpec {

  implicit val positive = Arbitrary(Gen.choose(0, 20))

  it should "drop" in {
    forAll { (n: Int, m: Int) =>
      val expected = (0 until n).toList
      Stream(expected:_*).drop(m).toList shouldBe expected.drop(m)
    }
  }

  it should "filter" in {
    forAll { (n: Int) =>
      val expected = (0 until n).toList
      val p = (x: Int) => x % 5 == 0

      Stream(expected:_*).filter(p).toList shouldBe expected.filter(p)
    }
  }

  it should "flatMap" in {
    forAll { (n: Int) =>
      val expected = (0 until n).toList
      val f = (x: Int) => Stream(x * 5)
      val g = (x: Int) => List(x * 5)

      Stream(expected:_*).flatMap(f).toList shouldBe expected.flatMap(g)
    }
  }

  it should "forAll" in {
    forAll { (n: Int) =>
      val expected = (0 until n).toList
      val p1 = (x: Int) => x < n / 2
      val p2 = (x: Int) => x <= n

      Stream(expected:_*).forAll(p1) shouldBe expected.forall(p1)
      Stream(expected:_*).forAll(p2) shouldBe expected.forall(p2)
    }
  }

  it should "headOption" in {
    forAll { (n: Int) =>
      val expected = (0 until n).toList

      Stream(expected:_*).headOption shouldBe expected.headOption
    }
  }

  it should "headOption2" in {
    forAll { (n: Int) =>
      val expected = (0 until n).toList

      Stream(expected:_*).headOption2 shouldBe expected.headOption
    }
  }

  it should "map" in {
    forAll { (n: Int) =>
      val expected = (0 until n).toList
      val f = (x: Int) => x * 5

      Stream(expected:_*).map(f).toList shouldBe expected.map(f)
    }
  }

  it should "take" in {
    forAll { (n: Int, m: Int) =>
      val expected = (0 until n).toList
      Stream(expected:_*).take(m).toList shouldBe expected.take(m)
    }
  }

  it should "takeWhile" in {
    forAll { (n: Int) =>
      val expected = (0 until n).toList
      val p = (n: Int) => n < 5
      Stream(expected:_*).takeWhile(p).toList shouldBe expected.takeWhile(p)
    }
  }

  it should "takeWhile2" in {
    forAll { (n: Int) =>
      val expected = (0 until n).toList
      val p = (n: Int) => n < 5
      Stream(expected:_*).takeWhile2(p).toList shouldBe expected.takeWhile(p)
    }
  }

  it should "toList" in {
    forAll { (n: Int) =>
      val expected = (0 until n).toList
      Stream(expected:_*).toList shouldBe expected
    }
  }
}
