package chapter_15

import java.io.{File, PrintWriter}

import chapter_15.Process._
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class ProcessSpec extends FlatSpec with Matchers {

  val stream = Stream(1, 2, 3, 4, 5, 6)
  val dStream = Stream(1.0, 2.6, 2.4, 0.4, 0.6, 0.2)

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
    sum_2(stream.map(_.toDouble)).toList shouldBe List(1.0, 3.0, 6.0, 10.0, 15.0, 21.0)
  }

  "Process.take" should "process first n elements" in {
    take(4)(stream).toList shouldBe stream.take(4).toList
    take(0)(stream).toList shouldBe Nil
  }

  "Process.drop" should "drop first n elements" in {
    drop(4)(stream).toList shouldBe stream.drop(4).toList
    drop(2)(stream).toList shouldBe stream.drop(2).toList
    drop(0)(stream).toList shouldBe stream.drop(0).toList
  }

  "Process.takeWhile" should "take elements while f is true" in {
    takeWhile[Int](_ < 3)(stream).toList shouldBe stream.takeWhile(_ < 3).toList
    takeWhile[Int](_ => true)(stream).toList shouldBe stream.toList
    takeWhile[Int](_ => false)(stream).toList shouldBe Nil
  }

  "Process.dropWhile" should "drop elements while f is true" in {
    dropWhile[Int](_ < 3)(stream).toList shouldBe stream.dropWhile(_ < 3).toList
    dropWhile[Int](_ => true)(stream).toList shouldBe Nil
    dropWhile[Int](_ => false)(stream).toList shouldBe stream.toList
  }

  "Process.count" should "count elements in the Stream" in {
    count[Int](stream.take(2).reverse).toList shouldBe List(0, 1, 2)
    count[Int](Stream.empty).toList shouldBe List(0)
    count[Int](stream.reverse).toList shouldBe (0 to stream.size).toList

    count_2[Int](stream.take(2).reverse).toList shouldBe List(1, 2)
    count_2[Int](Stream.empty).toList shouldBe Nil
    count_2[Int](stream.reverse).toList shouldBe (1 to stream.size).toList

    count_3[Int](stream.take(2).reverse).toList shouldBe List(1, 2)
    count_3[Int](Stream.empty).toList shouldBe Nil
    count_3[Int](stream.reverse).toList shouldBe (1 to stream.size).toList
  }

  "Process.mean" should "calculate mean of the Stream" in {
    mean(dStream).toList shouldBe List(1.0, 1.8, 2.0, 1.6, 1.4, 1.2)

    mean_2(dStream).toList shouldBe List(1.0, 1.8, 2.0, 1.6, 1.4, 1.2)
  }

  "Process.evenPlus5" should "filter even and increment by 5" in {
    evenPlus5(stream).toList shouldBe List(7, 9, 11)
  }

  "Process.map" should "map a process using given function" in {
    evenPlus5.map(_.toString)(stream).toList shouldBe List("7", "9", "11")
  }

  "Process.++" should "append 2 processes" in {
    liftOne((_: Int) + 5).++(filter(_ % 3 == 0))(stream).toList shouldBe List(6, 3, 6)
  }

  "Process.zipWithIndex" should "zip process with index" in {
    evenPlus5.zipWithIndex(stream).toList shouldBe List(7 -> 0, 9 -> 1, 11 -> 2)
    lift[Int, Int](identity).zipWithIndex(stream).toList shouldBe stream.toList.zipWithIndex
  }
  "Process.zip" should "zip two processes" in {
    lift(identity[Int]).zip(lift(_.toString))(stream).toList shouldBe stream.toList.map(x => x -> x.toString)
  }
  "Process.exists" should "should return true if condition is true" in {
    exists[Int](_ % 2 == 0)(stream).toList shouldBe List(false, true, true, true, true, true)
    exists_2[Int](_ % 2 == 0)(stream).toList shouldBe List(true)
    exists_2[Int](_ => false)(stream).toList shouldBe List(false)
  }
  "Process.toCelsius" should "read lines from file filter, convert to celsius and write to other file" in {
    val celsius = List("1", "2.0", " ", "3.0", "22.2", "#XXX")
    //----------------------
    val file = new File("target/tst.dat")
    file.deleteOnExit()
    file.createNewFile()
    val p = new PrintWriter(file)
    celsius.foreach(p.println)
    p.close()
    //----------------------
    val resultFile = new File("target/result.dat")
    resultFile.createNewFile()
    resultFile.deleteOnExit()
    //----------------------
    processFile(file, filter[String](_.trim.nonEmpty) |> filter(!_.startsWith("#")) |> lift(x => toCelsius(x.toDouble)), {
      new PrintWriter(resultFile)
    }) { (p, a) =>
      p.println(a)
      p
    }.run.close()
    //----------------------
    Source.fromFile(resultFile).getLines().toList shouldBe celsius.filter(s => s.trim.nonEmpty && !s.startsWith("#")).map(x => toCelsius(x.toDouble).toString)
  }

}
