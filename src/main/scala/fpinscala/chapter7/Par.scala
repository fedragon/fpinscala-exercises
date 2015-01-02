package fpinscala
package chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    (es: ExecutorService) =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](e: ExecutorService)(p: Par[A]): Future[A] = p(e)

  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar2(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    val reversed = ps.foldRight(unit(Nil: List[A])) {
      (p, acc) => map2(acc, p)(_ :+ _)
    }
    map(reversed)(_.reverse)
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = {
    map2(unit(ps), unit(f))((as, f) => as.filter(f))
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val idx = run(es)(n).get
      choices.toIndexedSeq(idx)(es)
    }

  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(map(a)(b => if(b) 0 else 1))(List(ifTrue, ifFalse))

  private case class UnitFuture[A](get: A) extends java.util.concurrent.Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(b: Boolean) = false
  }
}

object Parallelized {

  import Par._

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(fork(sum(l)), fork(sum(r)))(_ + _)
    }

  def max(ints: IndexedSeq[Int]): Par[Int] = {
    val f = (is: Seq[Int]) => is.reverse.head
    val sortedList = sortPar2(unit(ints.toList))
    map2(sortedList, unit(f))((a, g) => g(a))
  }

  def words(ps: List[String]): Par[Int] = {
    val ws = map(unit(ps))(_.flatMap(_.split(" ")))
    val f = (ws: List[String]) => ws.size
    map2(ws, unit(f))((xs, g) => g(xs))
  }
}
