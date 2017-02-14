package chapter_11

import org.scalatest.{FlatSpec, Matchers}
import chapter_11.Reader._
import chapter_11.Id._

class ReaderSpec extends FlatSpec with Matchers {

  val r1: Reader[Int, Int] = Reader.lift((_: Int) * 2)
  val r2: Reader[Int, Int] = Reader.lift((_: Int) * 3)

  "Reader" should "pass an argument to all the functions" in {
    val x: Kleisli[Id, Int, Int] = for {
      a <- r1
      b <- r2
    } yield a + b

    x(2).a shouldBe 10
    x(10).a shouldBe 10 * 2 + 10 * 3

  }

}
