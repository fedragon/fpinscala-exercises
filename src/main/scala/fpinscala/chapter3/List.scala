package fpinscala
package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
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
        if(n <= 0) list
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

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def length2[A](xs: List[A]): Int = foldRight(xs, 0)((_, b) => b + 1)

  @scala.annotation.tailrec
  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
    xs match {
      case Nil => z
      case Cons(y, ys) => foldLeft(ys, f(z, y))(f)
    }
  }

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def length3[A](xs: List[A]): Int = foldLeft(xs, 0)((a, _) => a + 1)

  def reverse[A](xs: List[A]): List[A] =
    foldLeft(xs, Nil: List[A])((a, b) => Cons(b, a))

  def foldLeft2[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
    val g = (a: A, b: B) => f(b, a)
    foldRight(xs, z)(g)
  }

  def foldRight2[A, B](xs: List[A], z: B)(f: (A, B) => B): B = {
    val g = (b: B, a: A) => f(a, b)
    foldLeft(xs, z)(g)
  }

  def append2[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys)(Cons(_, _))

  def concat[A](zs: List[List[A]]): List[A] = foldRight(zs, Nil: List[A])(append)

  def addOne(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, addOne(t))
    }

  def convert(xs: List[Double]): List[String] =
    xs match {
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, convert(t))
    }

  def map[A, B](xs: List[A])(f: A => B): List[B] =
    xs match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

  def filter[A](xs: List[A])(f: A => Boolean): List[A] = {
    def filt(ys: List[A], acc: List[A]): List[A] = {
      ys match {
        case Nil => acc
        case Cons(h, t) =>
          if(f(h)) filt(t, Cons(h, acc))
          else filt(t, acc)
      }
    }

    reverse(filt(xs, Nil))
  }

  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = {
    foldLeft(map(xs)(f), Nil: List[B])(append)
  }
}
