package fpinscala
package chapter2

object Fibonacci {
  def apply(n: Int): Int = {
    def fibo(x: Int, n_1: Int, n_2: Int): Int = {
      if(x == n) n_1 + n_2
      else {
        fibo(x + 1, n_2, n_1 + n_2)
      }
    }

    if(n == 0) 0
    else if(n == 1) 1
    else fibo(2, 0, 1)
  }
}
