package chapter_15

import org.scalatest.{FlatSpec, Matchers}
import chapter_15.Process._

class ProcessSpec extends FlatSpec with Matchers  {

  val stream = Stream(1, 2, 3, 4, 5, 6)

  "Process.liftOne" should "lift func to Process for 1 element" in {
    liftOne[Int, String](_.toString)(stream).toList shouldBe List("1")
    liftOne[Int, Int](_ + 1)(stream).toList shouldBe List(2)
  }

  "Process.lift" should "lift func to Process for all the elements" in {
    lift[Int, String](_.toString)(stream).toList shouldBe stream.map(_.toString).toList
    lift[Int, Int](_ + 1)(stream).toList shouldBe stream.map(_ + 1).toList
  }

  "Process.filter" should "create process which filters elements" in {
    filter[Int](_ % 2 == 0)(stream).toList shouldBe stream.filter(_ % 2 == 0).toList
    filter[Int](_ % 2 != 0)(stream).toList shouldBe stream.filter(_ % 2 != 0).toList
  }

  "Process.sum" should "accumulate sum of elements" in {
    sum(stream.map(_.toDouble)).toList shouldBe List(1.0, 3.0, 6.0, 10.0, 15.0, 21.0)
  }

  "Process.take" should "process first n elements" in {
    take(4)(stream).toList shouldBe stream.take(4).toList
    take(0)(stream).toList shouldBe Nil
  }

}
