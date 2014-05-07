package fpinscala
package chapter2

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ComposeSuite extends FunSuite with Matchers {

  test("should compose two functions") {
    val f = (b: String) => b.toInt
    val g = (a: Int) => a.toString
    
    Compose(f, g)(1) shouldBe 1
    Compose(g, f)("1") shouldBe "1"
  }
}
