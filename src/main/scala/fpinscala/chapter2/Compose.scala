package fpinscala
package chapter2

object Compose {
  def apply[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
