package chapter_5

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  def f = {
    println("Hello: 1")
    12
  }
  def s = {
    println("Hello: 2")
    13
  }

  val stream = Stream(f, s)

  "Stream.toList" should "convert stream to List" in {
    stream.toList shouldBe List(12, 13)
  }

  "Stream.take" should "take first n elements of a Stream" in {
    stream.take(2).toList shouldBe List(12, 13)
    stream.take(3).toList shouldBe List(12, 13)
    stream.take(1).toList shouldBe List(12)
    stream.take(0) shouldBe Empty
    Empty.take(1) shouldBe Empty
  }

  "Stream.takeWhile" should "take elements while condition is true" in {
    stream.takeWhile(_ => true).toList shouldBe List(12, 13)
    stream.takeWhile(_ % 2 == 0).toList shouldBe List(12)
    stream.takeWhile(_ % 2 != 0) shouldBe Empty
    stream.takeWhile(_ => false) shouldBe Empty
    (Empty: Stream[Int]).takeWhile(_ % 2 == 0) shouldBe Empty

    stream.takeWhileUsingFR(_ => true).toList shouldBe List(12, 13)
    stream.takeWhileUsingFR(_ % 2 == 0).toList shouldBe List(12)
    stream.takeWhileUsingFR(_ % 2 != 0) shouldBe Empty
    stream.takeWhileUsingFR(_ => false) shouldBe Empty
    (Empty: Stream[Int]).takeWhileUsingFR(_ % 2 == 0) shouldBe Empty
  }

  "Stream.forAll" should "return true if all the elements satisfy condition" in {
    stream.forAll(_ => true) shouldBe true
    stream.forAll(_ > 0) shouldBe true
    stream.forAll(_ % 2 == 0) shouldBe false
    stream.forAll(_ % 2 != 0) shouldBe false
    stream.forAll(_ => false) shouldBe false
    (Empty: Stream[Int]).forAll(_ => true) shouldBe true
    (Empty: Stream[Int]).forAll(_ => false) shouldBe true
  }

  "Stream.headOption" should "the first element of Stream if exists" in {
    stream.headOption shouldBe Some(f)
    Empty.headOption shouldBe None
    stream.headOptionUsingFR shouldBe Some(f)
    Empty.headOptionUsingFR shouldBe None
  }

}
