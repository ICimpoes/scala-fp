package chapter_9


sealed trait Res[+A] {
  def isSuccess: Boolean

  def isFailure: Boolean

  def toEither: Either[ParseError, A]

  def mapError(f: ParseError => ParseError): Res[A]

  def uncommit: Res[A]

  def addCommit(isCommitted: Boolean): Res[A]

  def advanceSuccess(n: Int): Res[A]
}

object Res {

  case class Fail(error: ParseError, isCommitted: Boolean = true) extends Res[Nothing] {
    val isSuccess: Boolean = false

    val isFailure: Boolean = true

    override def toEither: Either[ParseError, Nothing] = Left(error)

    override def mapError(f: (ParseError) => ParseError): Res[Nothing] = Fail(f(error), isCommitted)

    override def uncommit: Res[Nothing] = Fail(error, false)

    override def addCommit(committed: Boolean): Res[Nothing] =
      Fail(error, isCommitted || committed)

    override def advanceSuccess(n: Int): Res[Nothing] = this
  }

  case class Succ[A](get: A, length: Int) extends Res[A] {

    val isSuccess: Boolean = true

    val isFailure: Boolean = false

    override def toEither: Either[ParseError, A] = Right(get)

    override def mapError(f: (ParseError) => ParseError): Res[A] = this

    override def uncommit: Res[A] = this

    override def addCommit(isCommitted: Boolean): Res[A] = this

    override def advanceSuccess(n: Int): Res[A] = Succ(get, length + n)
  }

}