package chapter_9

import chapter_9.Result.{F, S}

import scala.util.matching.Regex

object SimpleParser {

  case class Location(input: String, offset: Int) {

    def slice(length: Int) = input.slice(offset, offset + length)

    val left = input.drop(offset)

    def move(length: Int): Location = this.copy(offset = offset + length)

  }

  type Parser[+A] = Location => Result[A]

  val p = new Parsers[F, Parser] {

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

    implicit def string: Parser[String] =
      token("\"[a-zA-Z]+\"".r)

    implicit def string(s: String): Parser[String] =
      (location: Location) => if (location.left.startsWith(s)) S(s, s.length) else F(s"Wrong string: Expected $s found ${location.left}")

    implicit def double: Parser[Double] =
      token("(-)?(\\d+)(\\.\\d*)?".r).map(_.toDouble)

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

object M extends App {

  type SimpParser = Map[String, Double]

  val input = """
               [
    |"qwe" : 9,
    |"wer" :    1.2,
    |
    |"asda"   :  44,
    |"xx" :  42
    |
    | ]
    |
  """.stripMargin.trim


  import SimpleParser._
  import p._

  val tupleParser: Parser[(String, Double)] = string.skipThat(':').map2(double)(_ -> _)

  println(p.run('['.skipThis(tupleParser.manyWithSeparator(",")).skipThat(']'))(input))

  println(run(p.string("aaacc").many.slice.map(_.toUpperCase))("aaaccaaaccaaaccsadds"))

  println(run(p.string("blah") ** p.impl.contextSensitive ** char('h').many.slice)("blah3aaahh"))
}