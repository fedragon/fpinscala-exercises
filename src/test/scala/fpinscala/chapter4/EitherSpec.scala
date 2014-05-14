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

  it should "sequence" in {
    Either.sequence(List(Right(1), Left("whoops"))) shouldBe Left("whoops")
    Either.sequence(List(Right(1), Right(2))) shouldBe Right(List(1, 2))
  }

  it should "traverse" in {
    Either.traverse(List(Right(1), Right(2)))(identity) shouldBe Right(List(1, 2))
  }
}
