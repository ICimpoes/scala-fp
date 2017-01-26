package chapter_9

import chapter_9.MyParser.Parser
import chapter_9.Res.{Fail, Succ}

import scala.util.matching.Regex

class MyParser extends Parsers[ParseError, Parser] {

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input)).toEither


  implicit def string(s: String): Parser[String] = scope(s"Expected: $s")(location => {
    if (location.input.startsWith(s)) Succ(s, s.length)
    else Fail(ParseError(Nil))
  })

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = location =>
    s1(location) match {
      case Fail(_, false) => s2(location)
      case r => r
    }

  implicit def regex(r: Regex): Parser[String] = location =>
    r.findPrefixOf(location.input) match {
      case Some(str) =>
        Succ(str, str.length)
      case _ =>
        Fail(ParseError(List(location -> s"${location.input} does not match: $r")))
    }

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = location =>
    p(location).mapError(_.push(location, msg))

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = location =>
    p(location).mapError(_.label(msg))

  override def succeed[A](a: A): Parser[A] = _ => Succ(a, 0)

  def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = location =>
    p(location) match {
      case Succ(a, la) =>
        f(a)(location.move(la))
          .addCommit(true)
          .advanceSuccess(la)
      case f: Fail =>
        f
    }

  def slice[A](p: Parser[A]): Parser[String] = location => {
    p(location) match {
      case Succ(_, l) =>
        Succ(location.slice(l), l)
      case f: Fail =>
        f
    }
  }

  override def attempt[A](p: Parser[A]): Parser[A] = location =>
    p(location).uncommit
}

object MyParser {

  type Parser[+A] = Location => Res[A]

}
