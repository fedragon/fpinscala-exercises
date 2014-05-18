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
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def fibs: Stream[Int] = {
    def f(n_2: Int, n_1: Int): Stream[Int] = {
      val next = n_2 + n_1
      cons(next, f(n_1, next))
    }

    cons(0, cons(1, f(0, 1)))
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
