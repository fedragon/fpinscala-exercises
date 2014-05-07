package fpinscala
package chapter2

object Currying {
  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => f(a, _)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
}
