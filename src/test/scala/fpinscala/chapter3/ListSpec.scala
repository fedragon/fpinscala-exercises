package fpinscala
package chapter3

import org.scalacheck.{Arbitrary, Gen}

object Helpers {
  import fpinscala.chapter3.{List => MyList, Nil => MyNil}
  import scala.collection.immutable.{List, Nil}

  implicit class ToScalaList[A](val xs: MyList[A]) extends AnyVal {
    def toScalaList: List[A] = {
      def convert(ys: MyList[A], acc: List[A]): List[A] =
        ys match {
          case MyNil => acc
          case Cons(h, t) => convert(t, h :: acc)
        }

      convert(xs, Nil).reverse
    }
  }
}

class ListSpec extends UnitSpec {

  import List._, Helpers._

  implicit val arbitraryIntList = Arbitrary(Gen.oneOf(List(1), List(1, 2, 3, 4)))
  implicit val arbitraryDoubleList = Arbitrary(Gen.oneOf(List(1.0), List(1.0, 2.0, 3.0, 4.0)))

  it should "return the tail" in {
    forAll { (xs: List[Int]) =>
      tail(xs).toScalaList shouldBe xs.toScalaList.tail
    }
  }

  it should "replace the head" in {
    forAll { (xs: List[Int], n: Int) =>
      setHead(xs, n).toScalaList shouldBe (n :: xs.toScalaList.tail)
    }
  }

  it should "drop the first n elements" in {
    forAll { (xs: List[Int], n: Int) =>
      drop(xs, n).toScalaList shouldBe (xs.toScalaList.drop(n))
    }
  }

  it should "drop elements as long as they match the predicate" in {
    val p = (x: Int) => x % 2 != 0

    forAll { (xs: List[Int], n: Int) =>
      dropWhile(xs, p).toScalaList shouldBe (xs.toScalaList.dropWhile(p))
    }
  }

  it should "return all elements but the last" in {
    forAll { (xs: List[Int]) =>
      init(xs).toScalaList shouldBe (xs.toScalaList.init)
    }
  }

  it should "count the length" in {
    forAll { (xs: List[Int]) =>
      length2(xs) shouldBe (xs.toScalaList.length)
    }
  }

  it should "fold left" in {
    val f = (a: Int, b: Int) => (a + b)

    forAll { (xs: List[Int]) =>
      foldLeft(xs, 0)(f) shouldBe (xs.toScalaList.foldLeft(0)(f))
    }
  }

  it should "sum, multiply and count length using foldLeft" in {
    forAll { (xs: List[Int]) =>
      sum3(xs) shouldBe (xs.toScalaList.sum)
      length3(xs) shouldBe (xs.toScalaList.length)
    }

    forAll { (xs: List[Double]) =>
      product3(xs) shouldBe (xs.toScalaList.foldLeft(1.0)(_ * _))
    }
  }

  it should "reverse a list" in {
    forAll { (xs: List[Int]) =>
      reverse(xs).toScalaList shouldBe (xs.toScalaList.reverse)
    }
  }

  it should "fold left using fold right" in {
    val f = (a: Int, b: Int) => (a + b)

    forAll { (xs: List[Int]) =>
      foldLeft2(xs, 0)(f) shouldBe (xs.toScalaList.foldLeft(0)(f))
    }
  }

  it should "fold right using fold left" in {
    val f = (a: Int, b: Int) => (a + b)

    forAll { (xs: List[Int]) =>
      foldRight2(xs, 0)(f) shouldBe (xs.toScalaList.foldLeft(0)(f))
    }
  }

  it should "append using fold left" in {
    forAll { (xs: List[Int], ys: List[Int]) =>
      append2(xs, ys).toScalaList shouldBe (xs.toScalaList ++ ys.toScalaList)
    }
  }

  it should "concat two lists" in {
    forAll { (xs: List[Int], ys: List[Int]) =>
      concat(List(xs, ys)).toScalaList shouldBe (xs.toScalaList ++ ys.toScalaList)
    }
  }

  it should "add 1 to all elements in the list" in {
    forAll { (xs: List[Int]) =>
      addOne(xs).toScalaList shouldBe (xs.toScalaList.map(_ + 1))
    }
  }

  it should "convert all elements in the list to Strings" in {
    forAll { (xs: List[Double]) =>
      convert(xs).toScalaList shouldBe (xs.toScalaList.map(_.toString))
    }
  }

  it should "map over a list" in {
    val f = (a: Int) => a + 1
    val g = (a: Int) => a.toString

    forAll { (xs: List[Int]) =>
      map(xs)(f).toScalaList shouldBe (xs.toScalaList.map(f))
      map(xs)(g).toScalaList shouldBe (xs.toScalaList.map(g))
    }
  }

  it should "filter a list" in {
    val f = (a: Int) => a % 2 == 0

    forAll { (xs: List[Int]) =>
      filter(xs)(f).toScalaList shouldBe (xs.toScalaList.filter(f))
    }
  }

  it should "flatMap a list" in {
    forAll { (xs: List[Int]) =>
      flatMap(xs)(List(_)).toScalaList shouldBe (xs.toScalaList.flatMap(scala.collection.immutable.List(_)))
    }
  }

  it should "filter a list using flatMap" in {
    val f = (a: Int) => a % 2 == 0

    forAll { (xs: List[Int]) =>
      filter2(xs)(f).toScalaList shouldBe (xs.toScalaList.filter(f))
    }
  }

  it should "add corresponding elements" in {
    forAll { (xs: List[Int], ys: List[Int]) =>
      addCorresponding(xs, ys).toScalaList shouldBe (xs.toScalaList.zip(ys.toScalaList).map(i => i._1 + i._2))
    }
  }

  it should "apply a function to corresponding elements" in {
    forAll { (xs: List[Int], ys: List[Int]) =>
      mapCorresponding(xs, ys)(_ + _).toScalaList shouldBe (xs.toScalaList.zip(ys.toScalaList).map(i => i._1 + i._2))
    }
  }
}
