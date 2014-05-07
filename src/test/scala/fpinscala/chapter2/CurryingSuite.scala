package fpinscala
package chapter2

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CurryingSuite extends FunSuite with Matchers {

  import Currying._

  test("should curry a function") {
    val f = (a: Int, b: Int) => a + b
    
    f(2, 3) shouldBe curry(f)(2)(3)
  }

  test("should uncurry a function") {
    val f = (a: Int) => (b: Int) => a + b
    
    f(2)(3) shouldBe uncurry(f)(2, 3)
  }
}
