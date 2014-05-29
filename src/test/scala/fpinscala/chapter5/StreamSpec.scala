package fpinscala
package chapter5

import org.scalacheck.{Arbitrary, Gen}

class StreamSpec extends UnitSpec {

  // arbitrary stream size: should always be a positive number
  implicit val positive = Arbitrary(Gen.choose(0, 20))

  it should "build a stream with constant values" in {
    forAll { (n: Int, m: Int) =>
      Stream.constant(1).take(m).toList shouldBe List.fill(m)(1)
      Stream.constant2(1).take(m).toList shouldBe List.fill(m)(1)
    }
  }

  it should "build a stream of ones" in {
    forAll { (n: Int, m: Int) =>
      val expected = (0 until m).map(_ => 1).toList
      Stream.ones.take(m).toList shouldBe expected
      Stream.ones2.take(m).toList shouldBe expected
    }
  }

  it should "build a stream from a number" in {
    forAll { (n: Int, m: Int) =>
      val expected = (0 until m).map(_ + n).toList
      Stream.from(n).take(m).toList shouldBe expected
      Stream.from2(n).take(m).toList shouldBe expected 
    }
  }

  it should "drop" in {
    forAll { (n: Int, m: Int) =>
      val expected = (0 until n).toList
      Stream(expected:_*).drop(m).toList shouldBe expected.drop(m)
    }
  }

  it should "build a Fibonacci sequence" in {
    val expected = List(0, 1, 1, 2, 3)
    Stream.fibs.take(5).toList shouldBe expected
    Stream.fibs2.take(5).toList shouldBe expected
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
      Stream(expected:_*).headOption2 shouldBe expected.headOption
    }
  }

  it should "map" in {
    forAll { (n: Int) =>
      val expected = (0 until n).toList
      val f = (x: Int) => x * 5

      Stream(expected:_*).map(f).toList shouldBe expected.map(f)
      Stream(expected:_*).mapViaUnfold(f).toList shouldBe expected.map(f)
    }
  }

  it should "take" in {
    forAll { (n: Int, m: Int) =>
      val expected = (0 until n).toList

      Stream(expected:_*).take(m).toList shouldBe expected.take(m)
      Stream(expected:_*).takeViaUnfold(m).toList shouldBe expected.take(m)
    }
  }

  it should "takeWhile" in {
    forAll { (n: Int) =>
      val expected = (0 until n).toList
      val p = (n: Int) => n < 5

      Stream(expected:_*).takeWhile(p).toList shouldBe expected.takeWhile(p)
      Stream(expected:_*).takeWhile2(p).toList shouldBe expected.takeWhile(p)
      Stream(expected:_*).takeWhileViaUnfold(p).toList shouldBe expected.takeWhile(p)
    }
  }

  it should "toList" in {
    forAll { (n: Int) =>
      val expected = (0 until n).toList
      Stream(expected:_*).toList shouldBe expected
    }
  }

  it should "unfold" in {
    def f(xy: (Int, Int)): Option[(Int, (Int, Int))] = {
      val next = xy._1 + xy._2
      Some((xy._1, (xy._2, next)))
    }

    Stream.unfold((0, 1))(f).take(5).toList shouldBe List(0, 1, 1, 2, 3)
  }

  it should "zipAll" in {
    Stream.empty.zipAll(Stream.empty).toList shouldBe List()
    Stream(0, 1, 2).zipAll(Stream(5, 6, 7)).toList shouldBe List((Some(0), Some(5)), (Some(1), Some(6)), (Some(2), Some(7)))
    Stream(0, 1).zipAll(Stream(5, 6, 7)).toList shouldBe List((Some(0), Some(5)), (Some(1), Some(6)), (None, Some(7)))
    Stream(0, 1, 2).zipAll(Stream(5, 6)).toList shouldBe List((Some(0), Some(5)), (Some(1), Some(6)), (Some(2), None))
  }
}
