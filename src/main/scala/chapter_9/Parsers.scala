package chapter_9

import chapter_8.{Gen, Prop}

trait Parsers[ParseError, Parser[+ _]] {
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  implicit def char(c: Char): Parser[Char] =
    string(c.toString).map(_.head)

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] = ???

  implicit def string(s: String): Parser[String] = ???

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def asCharParser[A](a: A)(implicit f: A => Parser[Char]): ParserOps[Char] = ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???

  def many[A](p: Parser[A]): Parser[List[A]] = ???

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = ???

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
  }


  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      in.forAll(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

  object impl {
    val numA: Parser[Int] = char('a').many.map(_.size)
  }

}