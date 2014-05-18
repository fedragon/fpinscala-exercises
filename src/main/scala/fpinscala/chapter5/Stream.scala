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

  def take(n: Int): Stream[A] =
    this match {
      case Empty => empty
      case Cons(h, t) =>
        if(n > 0) cons(h(), t().take(n - 1))
        else empty
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Empty => empty
      case Cons(h, t) =>
        lazy val hd = h()
        if(p(hd)) cons(hd, t().takeWhile(p))
        else empty
    }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else b)

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
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
