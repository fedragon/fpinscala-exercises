package fpinscala
package chapter3

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ListSpec extends FlatSpec with Matchers {

  import List._

  it should "return the tail" in {
    tail(Nil) shouldBe Nil
    tail(List(1, 2, 3)) shouldBe List(2, 3)
  }

  it should "replace the head" in {
    setHead(Nil, 4) shouldBe Nil
    setHead(List(1, 2, 3), 4) shouldBe List(4, 2, 3)
  }

  it should "drop the first n elements" in {
    drop(Nil, 2) shouldBe Nil
    drop(List(1, 2, 3, 4), 2) shouldBe List(3, 4)
  }

  it should "drop elements as long as they match the predicate" in {
    dropWhile(Nil, (x: Int) => true) shouldBe Nil
    dropWhile(List(1, 1, 3, 4), (x: Int) => x % 2 != 0) shouldBe List(4)
  }

  it should "return all elements but the last" in {
    init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
  }

  it should "count the length" in {
    length2(List(1, 2, 3, 4)) shouldBe 4
  }

  it should "fold left" in {
    foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
  }

  it should "sum, multiply and count length using foldLeft" in {
    sum3(List(1, 2, 3)) shouldBe 6
    product3(List(1, 2, 3, 4)) shouldBe 24
    length3(List(1, 2)) shouldBe 2
  }
}
