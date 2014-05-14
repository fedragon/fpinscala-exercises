package fpinscala
package chapter4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
      // case left => left fails with type mismatch:
      // found: fpinscala.chapter4.Either[E, A]
      // required: fpinscala.chapter4.Either[E, B]
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => Right(a)
      case Left(_) => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this match {
      case Right(x) => b map (y => f(x, y))
      case Left(e) => Left(e)
    }
  }
}

object Either {
    def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      a match {
        case Nil => Right(Nil)
        case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
      }

    def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
      traverse(a)(identity)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
