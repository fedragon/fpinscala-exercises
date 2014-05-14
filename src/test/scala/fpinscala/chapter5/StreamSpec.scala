package fpinscala
package chapter5

class StreamSpec extends UnitSpec {

  it should "convert to list" in {
    Empty.toList shouldBe Nil
    Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
  }

}
