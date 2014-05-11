package fpinscala
package chapter2

class CurryingSuite extends UnitSpec {

  import Currying._

  it should "curry a function" in {
    val f = (a: Int, b: Int) => a + b

    f(2, 3) shouldBe curry(f)(2)(3)
  }

  it should "uncurry a function" in {
    val f = (a: Int) => (b: Int) => a + b

    f(2)(3) shouldBe uncurry(f)(2, 3)
  }
}
