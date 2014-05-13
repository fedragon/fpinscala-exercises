package fpinscala
package chapter4

class EitherSpec extends UnitSpec {

  it should "map" in {
    Right(1).map(_ + 1) shouldBe Right(2)
  }

  it should "flatMap" in {
    Right(1).flatMap(x => Right(x + 1)) shouldBe Right(2)
  }

  it should "orElse" in {
    Left(1).orElse(Right(2)) shouldBe Right(2)
    Right(1).orElse(Right(2)) shouldBe Right(1)
  }

  it should "map2" in {
    Left(1).map2(Right(2))((_, x: Int) => x) shouldBe Left(1)
    Right(1).map2(Left(2))(_ + _) shouldBe Left(2)
    Right(1).map2(Right(2))(_ + _) shouldBe Right(3)
  }

}
