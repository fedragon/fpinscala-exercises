package fpinscala
package chapter7

import java.util.concurrent.{Executors, ExecutorService}

class NonBlockingSpec extends UnitSpec {

  val e = Executors.newFixedThreadPool(1)

  it should "run non blocking" in {
    import NonBlockingPar._
    val f = unit(1)
    NonBlockingPar.run(e)(f) shouldBe 1
  }

  it should "handle exceptions" in {
    import NonBlockingPar._
    val f: Par[Int] = (es: ExecutorService) => throw new IllegalStateException("foo bar")
    NonBlockingPar.run(e)(f)
  }

}
