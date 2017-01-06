package chapter_9

import chapter_8.{Gen, Prop}

trait Parsers[ParseError, Parser[+ _]] {
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  implicit def char(c: Char): Parser[Char] =
    string(c.toString).map(_.head)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???

  implicit def string(s: String): Parser[String] = ???

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def asCharParser[A](a: A)(implicit f: A => Parser[Char]): ParserOps[Char] = ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List[A]())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    or(map2(p, many(p))(_ :: _), succeed(List[A]()))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = ???

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = ???

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p, p2).map(f.tupled)

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String] = ???

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)

    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }


  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      in.forAll(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](ga: Gen[A], gs: Gen[String]): Prop =
      ga ** gs forAll { case (a, s) => run(succeed(a))(s) == Right(a) }

    def productLaw[A](ga: Gen[A], gs: Gen[String]): Prop =
      ga ** gs forAll { case (a, s) => run(succeed(a) ** succeed(a))(s) == Right(a -> a) }

  }

  object impl {
    val numA: Parser[Int] = char('a').many.slice.map(_.length)
    val numABs: Parser[(Int, Int)] = char('a').many.slice.map(_.length) ** char('b').many1.slice.map(_.length)
  }

}