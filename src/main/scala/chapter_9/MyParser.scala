package chapter_9

import chapter_9.MyParser.Parser
import chapter_9.Result.{F, S}
import chapter_9.SimpleParser.Location

import scala.util.matching.Regex

class MyParser extends Parsers[ParseError, Parser] {

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    p(input) match {
      case S(a, int) => Right(a)
      case F(msg) => Left(Location(input).toError(msg))
    }
  }

  implicit def string(s: String): Parser[String] = input => {
    if (input.startsWith(s)) S(s, s.length)
    else F(s"Expected: $s. Found: $input")
  }

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = input => {
    val res = s1(input)
    if (res.isSuccess)
      res
    else
      s2(input)
  }

  implicit def regex(r: Regex): Parser[String] = input => {
    r.findPrefixOf(input) match {
      case Some(str) =>
        S(str, str.length)
      case _ =>
        F(s"${input} does not match: $r")
    }
  }

  def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = input => {
    p(input) match {
      case S(a, la) =>
        f(a)(input.drop(la)) match {
          case S(b, lb) =>
            S(b, la + lb)
          case failure =>
            failure
        }
      case f: F =>
        f
    }
  }

  def slice[A](p: Parser[A]): Parser[String] = input => {
    p(input) match {
      case S(_, l) =>
        S(input.take(l), l)
      case f: F =>
        f
    }
  }
}

object MyParser {

  type Parser[+A] = String => Result[A]

}
