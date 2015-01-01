package fpinscala
package chapter7

import java.util.concurrent.{Executors, ExecutorService}

import Par._

class ParSpec extends UnitSpec {

  it should "sequence a list of Par into a Par of list" in {
    val e = Executors.newFixedThreadPool(1)

    val expected = unit(List(1, 2))
    val actual = Par.sequence(List(unit(1), unit(2)))
    Par.run(e)(actual) shouldBe Par.run(e)(expected)
  }

}
