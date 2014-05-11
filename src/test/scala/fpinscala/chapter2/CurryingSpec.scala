package fpinscala
package chapter2

class CurryingSuite extends UnitSpec {

  import Currying._

  it should "curry a function" in {
    val f = (a: Int, b: Int) => a + b

    forAll { (x: Int, y: Int) =>
      f(x, y) shouldBe curry(f)(x)(y)
    }
  }

  it should "uncurry a function" in {
    val f = (a: Int) => (b: Int) => a + b

    forAll { (x: Int, y: Int) =>
      f(x)(y) shouldBe uncurry(f)(x, y)
    }
  }
}
