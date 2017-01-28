package chapter_9

import chapter_9.Res.{Fail, Succ}

import scala.util.matching.Regex

object MyParser {

  type Parser[+A] = Location => Res[A]

  val parser = new Parsers[ParseError, Parser] {

    def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
      p(Location(input)).toEither


    implicit def string(s: String): Parser[String] = scope(in => s"Expected: `$s` found `$in`")(location => {
      if (location.left.startsWith(s)) Succ(s, s.length)
      else Fail(ParseError(List(location -> s"Expected: `$s` found `${location.left}`")))
    })

    def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = location =>
      attempt(s1)(location) match {
        case Fail(_, false) => s2(location)
        case r => r
      }

    implicit def regex(r: Regex): Parser[String] = scope(in => s"`$in` does not match: `$r`")(location =>
      r.findPrefixOf(location.left) match {
        case Some(str) =>
          Succ(str, str.length)
        case _ =>
          Fail(ParseError(List(location -> s"`${location.left}` does not match: `$r`")))
      })

    override def errorLocation(e: ParseError): Location = e.latest.get._1

    override def errorMessage(e: ParseError): String = e.latest.get._2

    override def scope[A](msg: String => String)(p: Parser[A]): Parser[A] = location =>
      p(location).mapError(_.push(location, msg(location.left)))

    override def label[A](msg: String => String)(p: Parser[A]): Parser[A] = location =>
      p(location).mapError(_.label(msg(location.left)))

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
}
