package chapter_9

import chapter_9.SimpleParser.Location

case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)
}

sealed trait Res[+A] {
  def isSuccess: Boolean

  def isFailure: Boolean

  def toEither: Either[ParseError, A]

  def mapError(f: ParseError => ParseError): Res[A]
}

object Res {

  case class Fail(error: ParseError) extends Res[Nothing] {
    val isSuccess: Boolean = false

    val isFailure: Boolean = true

    override def toEither: Either[ParseError, Nothing] = Left(error)

    override def mapError(f: (ParseError) => ParseError): Res[Nothing] = Fail(f(error))
  }

  case class Succ[A](get: A, length: Int) extends Res[A] {
    val isSuccess: Boolean = true

    val isFailure: Boolean = false

    override def toEither: Either[ParseError, A] = Right(get)

    override def mapError(f: (ParseError) => ParseError): Res[A] = this
  }

}