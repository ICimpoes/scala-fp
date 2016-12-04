package object chapter_4 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)


  def variance(xs: Seq[Double]): Option[Double] = {
    val maybeMean = mean(xs)

    maybeMean.flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }

  }

  def map2[A, B, C](maybeA: Option[A], maybeB: Option[B])(f: (A, B) => C): Option[C] =
    maybeA.flatMap(a => maybeB.map(b => f(a, b)))

  def sequence[A](list: List[Option[A]]): Option[List[A]] = {
    list.foldLeft(Some(Nil): Option[List[A]]) {
      case (Some(l), Some(el)) =>
        Some(l.:+(el))
      case _ =>
        None: Option[List[A]]
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h).flatMap( hh => traverse(t)(f).map(_.+:(hh)))
  }

  def sequenceUsingTraverse[A](list: List[Option[A]]): Option[List[A]] = {
    traverse(list)(identity)
  }

  def eitherSequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    eitherTraverse(es)(identity)

  def eitherTraverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).flatMap( hh => eitherTraverse(t)(f).map(_.+:(hh)))
  }


}
