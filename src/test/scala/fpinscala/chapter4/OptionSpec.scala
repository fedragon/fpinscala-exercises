package fpinscala
package chapter4

class OptionSpec extends UnitSpec {

  it should "get the value or return the default value" in {
    None.getOrElse(1) shouldBe 1
    Some(1).getOrElse(2) shouldBe 1
  }

  it should "flatMap" in {
    val f = (a: Int) => Some(a + 1)

    (None: Option[Int]).flatMap(f) shouldBe None
    Some(1).flatMap(f) shouldBe Some(2)
  }

  it should "map" in {
    val f = (a: Int) => a + 1

    (None: Option[Int]).map(f) shouldBe None
    Some(1).map(f) shouldBe Some(2)
  }

  it should "getOrElse" in {
    (None: Option[Int]).getOrElse(2) shouldBe 2
    Some(1).getOrElse(Some(2)) shouldBe 1
  }

  it should "orElse" in {
    (None: Option[Int]).orElse(Some(1)) shouldBe Some(1)
    Some(1).orElse(Some(2)) shouldBe Some(1)
  }

  it should "filter" in {
    val f = (a: Int) => a == 1

    (None: Option[Int]).filter(f) shouldBe None
    Some(1).filter(f) shouldBe Some(1)
    Some(2).filter(f) shouldBe None
  }

  it should "map two options" in {
    def f (a: Int, b: Int) = a + b

    Option.map2(None: Option[Int], Some(1))(f) shouldBe None
    Option.map2(Some(1), Some(2))(f) shouldBe Some(3)
  }

  it should "return a sequence of options" in {
    Option.sequence(List(Some(1), None: Option[Int])) shouldBe None
    Option.sequence(List(Some(1), Some(2))) shouldBe Some(List(1, 2))
  }

}
