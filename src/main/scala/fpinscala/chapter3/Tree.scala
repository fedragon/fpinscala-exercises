package fpinscala
package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int =
    t match {
      case Branch(l, r) => size(l) + size(r)
      case Leaf(_) => 1
    }

  def maximum(t: Tree[Int]): Int = {

    def maximum(st: Tree[Int], m: Int): Int = {
      st match {
        case Leaf(v) => m.max(v)
        case Branch(l, r) => maximum(l, m).max(maximum(r, m))
      }
    }

    maximum(t, Int.MinValue)
  }

  def depth[A](t: Tree[A]): Int = {
    def traverse(st: Tree[A], d: Int): Int = {
      st match {
        case Leaf(v) => d
        case Branch(l, r) => traverse(l, d + 1).max(traverse(r, d + 1))
      }
    }

    traverse(t, 0)
  }
}
