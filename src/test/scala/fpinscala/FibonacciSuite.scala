package fpinscala

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FibonacciSuite extends FunSuite with Matchers {

  test("should create the correct sequence") {
    Fibonacci(0) shouldBe 0
    Fibonacci(1) shouldBe 1
    Fibonacci(2) shouldBe 1
    Fibonacci(3) shouldBe 2
    Fibonacci(4) shouldBe 3
    Fibonacci(5) shouldBe 5
  }
}
