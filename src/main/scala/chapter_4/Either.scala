package chapter_4


sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case l: Left[E] => l
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case l: Left[E] => l
    case Right(v) => f(v)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case r@Right(_) => r
    case _ => b
  }

  def get: A = this match {
    case Right(v) => v
    case _ => throw new Exception("Cannot get from left side")
  }

  def map2[EE >: E, B, C](maybeB: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this -> maybeB match {
    case (Right(a), Right(b)) => Right(f(a, b))
    case (Right(a), l@Left(_)) => l
    case (a@Left(_), _) => a
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

