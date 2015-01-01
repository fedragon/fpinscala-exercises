package fpinscala
package chapter7

import java.util.concurrent.{Executors, ExecutorService}

class ParSpec extends UnitSpec {

  val e = Executors.newFixedThreadPool(1)

  it should "sequence a list of Par into a Par of list" in {
    val expected = Par.unit(List(1, 2))
    val actual = Par.sequence(List(Par.unit(1), Par.unit(2)))

    Par.run(e)(actual) shouldBe Par.run(e)(expected)
  }

  it should "filter a list of Par" in {
    val expected = Par.unit(List(2))
    val actual = Par.parFilter(List(1, 2, 3))(_ % 2 == 0)

    Par.run(e)(actual) shouldBe Par.run(e)(expected)
  }

  it should "find the maximum value in a list" in {
    val expected = Par.unit(3)
    val actual = Parallelized.max(Vector(1, 2, 3))

    Par.run(e)(actual) shouldBe Par.run(e)(expected)
  }

}
