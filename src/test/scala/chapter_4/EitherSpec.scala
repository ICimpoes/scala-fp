package chapter_4

import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {

  val l: Either[String, Int] = Left("Left")

  val r: Either[String, Int] = Right(2)

  "Either.map" should "apply the given function on either" in {
    l.map(_ + 1) shouldBe l
    r.map(_ + 1) shouldBe Right(3)
  }
  "Either.flatMap" should "apply the given function on either" in {
    l.flatMap(x => Right(x + 1)) shouldBe l
    r.flatMap(x => Right(x + 1)) shouldBe Right(3)
    r.flatMap(x => Left(s"right value $x")) shouldBe Left("right value 2")
  }
  "Either.orElse" should "return default on Left" in {
    l.orElse(Right(1)) shouldBe Right(1)
    l.orElse(Left("bla")) shouldBe Left("bla")
    r.orElse(Right(3)) shouldBe r
    r.orElse(Left("bla")) shouldBe r
  }
  "map2" should "map over two either" in {
    l.map2(r)(_ + _) shouldBe l
    r.map2(l)(_ + _) shouldBe l
    r.map2(Right(4))(_ + _) shouldBe Right(6)
  }
  "sequence" should "return either of list" in {
    eitherSequence(Nil) shouldBe Right(Nil)
    eitherSequence(List(l, r, Left("second left"))) shouldBe l
    eitherSequence(List(r, r)) shouldBe Right(List(2, 2))
    eitherSequence(List(r, Right(12))) shouldBe Right(List(2, 12))
  }
  "traverse" should "return option of list" in {
    eitherTraverse(Nil)(_ => Left("left")) shouldBe Right(Nil)
    eitherTraverse(List(1, 2, 3))(Right(_)) shouldBe Right(List(1, 2, 3))
    eitherTraverse(List(1, 2, 3))(x => Left(s"left $x")) shouldBe Left("left 1")
  }

}
