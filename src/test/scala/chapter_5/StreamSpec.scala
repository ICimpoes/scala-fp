package chapter_5

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  def f = {
    12
  }

  def s = {
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

  "Stream.map" should "map all the elements of stream" in {
    stream.map(_ + 2).toList shouldBe List(14, 15)
    Empty.map(identity) shouldBe Empty
    stream.headOptionUsingFR shouldBe Some(f)
    Empty.headOptionUsingFR shouldBe None
  }

  "Stream.filter" should "filter elements of the stream" in {
    stream.filter(_ % 2 == 0).toList shouldBe List(12)
    stream.filter(_ => false) shouldBe Empty
    stream.filter(_ => true).toList shouldBe stream.toList
    Empty.filter(_ => true) shouldBe Empty
    Empty.filter(_ => false) shouldBe Empty
  }

  "Stream.flatMap" should "flat map all the elements" in {
    stream.flatMap(x => Stream(x, x)).toList shouldBe List(12, 12, 13, 13)
    stream.flatMap(x => Empty) shouldBe Empty
    Stream.empty[Int].flatMap(x => Stream(x)) shouldBe Empty
  }

  "Stream.append" should "append one stream to another" in {
    stream.append(Stream(1, 2, 3)).toList shouldBe stream.toList.++(List(1, 2, 3))
    stream.append(Empty).toList shouldBe stream.toList
    Empty.append(Stream(1, 2, 3)).toList shouldBe List(1, 2, 3)
  }

  "Stream.find" should "find an element in the stream" in {
    stream.find(_ % 2 == 0) shouldBe Some(12)
    stream.find(_ % 2 != 0) shouldBe Some(13)
    stream.find(_ => false) shouldBe None
    Empty.find(_ => true) shouldBe None
  }

  "Stream" should "be infinite" in {
    lazy val x: Stream[Int] = Stream.cons(1, x)
    x.take(100).toList shouldBe List.fill(100)(1)
    x.exists(_ == 1) shouldBe true
    x.map(_ + 1).exists(_ == 2) shouldBe true
    intercept[StackOverflowError] {
      x.map(_ + 1).exists(_ == 1)
    }
  }

  "Stream.constant" should "generate an infinite stream of a constant" in {
    Stream.constant("cons").take(100).toList shouldBe List.fill(100)("cons")
  }

  "Stream.from" should "generate an infinite stream int stating from n" in {
    Stream.from(10).take(6).toList shouldBe List(10, 11, 12, 13, 14, 15)
  }

  "Stream.fibs" should "generate Fibonacci numbers" in {
    Stream.fibs().take(10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
  }

  "Stream.unfold" should "generate s stream" in {
    Stream.unfold(0)(x => if (x < 11) Some(x, x + 1) else None).toList shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    Stream.unfold("0")(x => if (x.length < 5) Some(x, s"$x${x.last.toString.toInt + 1}") else None).toList shouldBe List("0", "01", "012", "0123")
    Stream.unfold(1)(_ => None) shouldBe Empty

  }
}
