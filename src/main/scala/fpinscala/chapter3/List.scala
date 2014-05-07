package fpinscala
package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

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
}
