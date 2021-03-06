package chapter_9

import chapter_9.Result.{F, S}

import scala.util.matching.Regex

object SimpleParser {

  type Parser[+A] = Location => Result[A]

  val parser = new Parsers[F, Parser] {

    def run[A](p: Parser[A])(input: String): Either[F, A] =
      p(Location(input, 0)).toEither

    def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] =
      (l: Location) => {
        val s1Res = s1(l)
        if (s1Res.isSuccess)
          s1Res
        else
          s2(l)
      }

    override def succeed[A](a: A): Parser[A] = _ => S(a, 0)

    implicit def string(s: String): Parser[String] =
      (location: Location) => if (location.left.startsWith(s)) S(s, s.length) else F(s"Wrong string: Expected $s found ${location.left}")

    implicit def regex(r: Regex): Parser[String] =
      (l: Location) =>
        r.findPrefixOf(l.left) match {
          case Some(str) =>
            S(str, str.length)
          case _ =>
            F(s"${l.left} does not match: $r")
        }

    def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = (l: Location) => {
      p(l) match {
        case S(a, la) =>
          f(a)(l.move(la)) match {
            case S(b, lb) =>
              S(b, la + lb)
            case fb => fb
          }
        case fa: F =>
          fa
      }
    }

    def slice[A](p: Parser[A]): Parser[String] =
      l => p(l) match {
        case S(_, length) =>
          S(l.slice(length), length)
        case f: F =>
          f
      }
  }

}

sealed trait Result[+A] {
  def isSuccess: Boolean

  def isFailure: Boolean

  def toEither: Either[F, A]
}

object Result {

  case class F(descr: String) extends Result[Nothing] {
    val isSuccess: Boolean = false

    val isFailure: Boolean = true

    override def toEither: Either[F, Nothing] = Left(this)
  }

  case class S[A](get: A, length: Int) extends Result[A] {
    val isSuccess: Boolean = true

    val isFailure: Boolean = false

    override def toEither: Either[F, A] = Right(get)
  }

}
