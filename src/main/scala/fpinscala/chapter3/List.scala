package fpinscala
package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](xs: List[A]): List[A] =
    xs match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }

  def setHead[A](xs: List[A], head: A): List[A] =
    xs match {
      case Nil => Nil
      case Cons(_, tail) => Cons(head, tail)
    }

  def drop[A](xs: List[A], n: Int): List[A] =
    xs match {
      case Nil => Nil
      case list @ Cons(_, tail) =>
        if(n == 0) list
        else drop(tail, n - 1)
    }

  def dropWhile[A](xs: List[A], p: A => Boolean): List[A] =
    xs match {
      case Nil => Nil
      case list @ Cons(head, tail) =>
        if(p(head)) dropWhile(tail, p)
        else list
    }

  def init[A](xs: List[A]): List[A] = {
    def init0[A](ys: List[A], acc: List[A]): List[A] = {
      ys match {
        case Nil | Cons(_, Nil) => acc
        case Cons(h, t) => init0(t, Cons(h, acc))
      }
    }

    def reverse(ys: List[A], acc: List[A]): List[A] =
      ys match {
        case Nil => acc
        case Cons(h, t) => reverse(t, Cons(h, acc))
      }

    reverse(init0(xs, Nil), Nil)
  }

  def length2[A](xs: List[A]): Int =
    foldRight(xs, 0)((_, b) => b + 1)

  @scala.annotation.tailrec
  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
    xs match {
      case Nil => z
      case Cons(y, ys) => foldLeft(ys, f(z, y))(f)
    }
  }
}
