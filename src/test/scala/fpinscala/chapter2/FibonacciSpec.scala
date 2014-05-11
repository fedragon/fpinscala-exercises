package fpinscala
package chapter2

class FibonacciSuite extends UnitSpec {

  it should "create the correct sequence" in {
    Fibonacci(0) shouldBe 0
    Fibonacci(1) shouldBe 1
    Fibonacci(2) shouldBe 1
    Fibonacci(3) shouldBe 2
    Fibonacci(4) shouldBe 3
    Fibonacci(5) shouldBe 5
  }
}
