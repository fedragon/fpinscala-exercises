package fpinscala
package chapter5

import Stream._

sealed trait Stream[+A] {

  def append[B >: A](other: => Stream[B]): Stream[B] =
    foldRight(other)(cons(_, _))

  def drop(n: Int): Stream[A] =
    this match {
      case Empty => empty
      case Cons(h, t) =>
        if(n > 0) t().drop(n -1)
        else this
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else b)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean =
    this match {
      case Empty => true
      case Cons(h, t) =>
        p(h()) && t().forAll(p)
    }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def headOption2: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 =>
        cons(h(), t().take(n - 1))
      case _ => empty
    }

  // FIXME Only use unfold!
  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), m) if m > 0 => Some((h(), (t(), m - 1)))
      case _ => None
    }
  }

  // FIXME Only use unfold!
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if(p(h())) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else b)

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if(p(h())) => Some((h(), t()))
      case _ => None
    }

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        val value = (Some(h1()), Some(h2()))
        val state = (t1(), t2())
        Some((value, state))
      case (Cons(h, t), Empty) =>
        val value = (Some(h()), None)
        val state = (t(), empty)
        Some((value, state))
      case (Empty, Cons(h, t)) =>
        val value = (None, Some(h()))
        val state = (empty, t())
        Some((value, state))
      case (Empty, Empty) =>
        None
    }
  }
}

object Stream {

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constant2[A](a: A): Stream[A] =
    unfold(a)((x: A) => Some((x, x)))

  def empty[A]: Stream[A] = Empty

  def fibs: Stream[Int] = {
    def f(f0: Int, f1: Int): Stream[Int] = {
      val next = f0 + f1
      cons(next, f(f1, next))
    }

    cons(0, cons(1, f(0, 1)))
  }

  def fibs2: Stream[Int] = {
    unfold((0, 1)) {
      case (f0, f1) => Some((f0, (f1, f0 + f1)))
    }
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def from2(n: Int): Stream[Int] =
    unfold(n)((x: Int) => Some((x, x + 1)))

  val ones: Stream[Int] = Stream.cons(1, ones)

  val ones2: Stream[Int] = unfold(1)((_: Int) => Some((1, 1)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Empty
      case Some((value, state)) =>
        cons(value, unfold(state)(f))
    }
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
