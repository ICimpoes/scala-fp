package chapter_12

sealed trait Validation[+E, +A]

object Validation {

  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      fa -> fb match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (f: Failure[E], Success(_)) => f
        case (Success(_), f: Failure[E]) => f
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1.:+(h2).++(t2))
      }
  }

}

case class Failure[E](head: E, tail: Vector[E] = Vector())
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]