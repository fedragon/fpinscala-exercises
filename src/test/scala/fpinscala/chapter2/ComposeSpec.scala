package fpinscala
package chapter2

class ComposeSuite extends UnitSpec {

  it should "compose two functions" in {
    val f = (b: String) => b.toInt
    val g = (a: Int) => a.toString

    Compose(f, g)(1) shouldBe 1
    Compose(g, f)("1") shouldBe "1"
  }
}
