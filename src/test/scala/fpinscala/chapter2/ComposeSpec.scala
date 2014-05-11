package fpinscala
package chapter2

class ComposeSuite extends UnitSpec {

  it should "compose two functions" in {
    val f = (b: String) => b.toInt
    val g = (a: Int) => a.toString

    forAll { (n: Int) =>
      Compose(f, g)(n) shouldBe n

      val s = n.toString
      Compose(g, f)(s) shouldBe s
    }
  }
}
