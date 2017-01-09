package chapter_9

import chapter_9.Result.{F, S}

import scala.util.matching.Regex

object SimpleParser {


  type Parser[+A] = String => Result[A]


  val p = new Parsers[F, Parser] {

    def run[A](p: Parser[A])(input: String): Either[F, A] =
      p(input).toEither

    def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] =
      (s: String) => {
        val s1Res = s1(s)
        if (s1Res.isSuccess)
          s1Res
        else
          s2(s)
      }

    implicit def string(s: String): Parser[String] =
      s => S(s)

    implicit def double(s: String): Parser[Double] =
      regex("(-)?(\\d+)(\\.\\d*)?".r).map(_.toDouble)

    implicit def regex(r: Regex): Parser[String] =
      (s: String) =>
        s match {
          case r(g1) =>
            S(g1)
          case _ =>
            F("does not match")
        }

    def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = (s: String) => {
      p(s) match {
        case S(v) =>
          println(v)
          f(v)(s)
        case f: F =>
          f
      }
    }

    def slice[A](p: Parser[A]): Parser[String] =
      p.map(_.toString)
  }

}

trait Result[+A] {
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
  case class S[A](get: A) extends Result[A] {
    val isSuccess: Boolean = true

    val isFailure: Boolean = false

    override def toEither: Either[F, A] = Right(get)
  }
}

object M extends App {

  type SimpParser = Map[String, Double]


  """
    |"a" : 1,
    |"b: : 2
  """.stripMargin




  import SimpleParser._


  //WIP
  println(p.run(p.impl.numA)("aaaB"))


}