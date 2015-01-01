package fpinscala
package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] = {
    def seq(s: S, xs: List[State[S, A]], acc: List[A]): (List[A], S) = {
      xs match {
        case Nil => (acc.reverse, s)
        case hd :: tl =>
          val (a, s2) = hd.run(s)
          seq(s2, tl, a :: acc)
      }
    }

    State((s: S) => seq(s, rs, Nil: List[A]))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

import State._

case class State[S, +A](run: S => (A,S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](that: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => that.map(b => f(a, b)))
}

object RNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  type State[S, +A] = S => (A, S)
  // type Rand[+A] = RNG => (A, RNG)
  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt

  def double(rng: RNG): (Double, RNG) = {
    val (value, state) = nonNegativeInt(rng)

    (value.toDouble / Int.MaxValue, state)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, state) = nonNegativeInt(rng)
    val (d, state2) = double(rng)

    ((i, d), state2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (id, state) = intDouble(rng)

    (id.swap, state)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, state) = double(rng)
    val (d2, state2) = double(state)
    val (d3, state3) = double(state2)

    ((d1, d2, d3), state3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 until count).foldLeft((Nil: List[Int], rng)) { (acc, _) =>
      val (is, state) = acc
      val (i, newState) = state.nextInt

      (i :: is, newState)
    }
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[S, A, B](a: S => (A,S))(f: A => B): S => (B,S) = { rng =>
    val (s, rng2) = a(rng)
    (f(s), rng2)
  }

  def doubleWithMap(rng: RNG): (Double, RNG) =
    (map(nonNegativeInt)(_.toDouble / Int.MaxValue))(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)

    (f(a, b), rng3)
  }

  def intDoubleWithMap2(rng: RNG): ((Int, Double), RNG) =
    map2(nonNegativeInt, double)((_, _))(rng)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def randIntDouble = both(int, double)

  def randDoubleInt = both(double, int)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = { rng =>
    def seq(r: RNG, xs: List[Rand[A]], acc: List[A]): Rand[List[A]] = {
      if(xs.isEmpty) unit(acc)
      else {
        val (a, r2) = xs.head(r)
        seq(r2, xs.tail, a :: acc)
      }
    }

    seq(rng, rs, Nil: List[A])(rng)
  }

  def intsWithSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = rng =>
    flatMap(s)(a => unit(f(a)))(rng)

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}
