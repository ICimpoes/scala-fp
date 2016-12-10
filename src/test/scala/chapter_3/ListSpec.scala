package chapter_3

import org.scalatest.{FlatSpec, Matchers}
import chapter_3.List._

class ListSpec extends FlatSpec with Matchers {

  "List.init" should "remove the last element from the list" in {
    val l = List(1, 2, 3, 4)
    init(l) shouldBe List(1, 2, 3)
    intercept[RuntimeException](init(Nil))
  }

  "List.product2" should "calculate product" in {
    val l = List[Double](1, 2, 0, 4, 1, 1, 2, 3, 4)
    product2(l) shouldBe 0
  }
  "foldRight on List" should "reconstruct a list" in {
    foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
  }
  "List.length" should "return length of the list" in {
    List.length(List("1", "2", "2")) shouldBe 3
    List.length(Nil) shouldBe 0
  }
  "List.foldLeft" should "work correctly" in {
    List.foldLeft(List(1, 2, 3, 4, 5), "Start: ")((s, i) => s"$s, $i") shouldBe "Start: , 1, 2, 3, 4, 5"
    List.foldLeft(List(1, 2, 3, 4, 5), 0)(_ - _) shouldBe -15
  }
  "List.foldRight" should "work correctly" in {
    List.foldRight(List(1, 2, 3, 4), 2)(_ - _) shouldBe 0
  }
  "List.foldRightUsingFoldLeft" should "work correctly" in {
    List.foldRightUsingFoldLeft(List(1, 2, 3, 4), 2)(_ - _) shouldBe 0
  }

  "List.foldRightLazy" should "make short-circuit" in {
    var nrOfCalls = 0
    def prod(a: Double, b: => Double): Double = {
      nrOfCalls += 1
      if (a == 0.0) 0 else a * b
    }
    def prod2(a: Double, b: Double): Double = {
      nrOfCalls += 1
      if (a == 0.0) 0 else a * b
    }
    List.foldRightLazy(List[Double](1, 2, 0, 4, 5, 1, 1, 2), 1.0)(prod) shouldBe 0
    nrOfCalls shouldBe 3
    nrOfCalls = 0
    List.foldRight(List[Double](1, 2, 0, 4, 5, 1, 1, 2), 1.0)(prod2) shouldBe 0
    nrOfCalls shouldBe 8
  }
  "List.sum2" should "return sum of all the numbers in the list" in {
    List.sum2(List[Double](1, 2, 0, 4)) shouldBe 7
  }
  "List.product3" should "return product of all the numbers in the list" in {
    List.product3(List[Double](1, 2, 4)) shouldBe 8
  }
  "List.reverse" should "reverse given list" in {
    List.reverse(List(1, 2, 4)) shouldBe List(4, 2, 1)
  }
  "List.appendUsingFR" should "append two lists" in {
    List.appendUsingFR(List(1, 2, 4), List(3, 4)) shouldBe List(1, 2, 4, 3, 4)
    List.appendUsingFR(Nil, List(3, 4)) shouldBe List(3, 4)
    List.appendUsingFR(List(1, 2, 4), Nil) shouldBe List(1, 2, 4)
    List.appendUsingFR(Nil, Nil) shouldBe Nil
  }
  "List.appendUsingFL" should "append two lists" in {
    List.appendUsingFL(List(1, 2, 4), List(3, 4)) shouldBe List(1, 2, 4, 3, 4)
    List.appendUsingFL(Nil, List(3, 4)) shouldBe List(3, 4)
    List.appendUsingFL(List(1, 2, 4), Nil) shouldBe List(1, 2, 4)
    List.appendUsingFL(Nil, Nil) shouldBe Nil
  }
  "List.concat" should "concat list of lists" in {
    List.concat(List(List(1, 2, 4), List(3, 4))) shouldBe List(1, 2, 4, 3, 4)
    List.concat(List(Nil, List(3, 4))) shouldBe List(3, 4)
    List.concat(List(List(1, 2, 4), Nil)) shouldBe List(1, 2, 4)
    List.concat(List(Nil, Nil)) shouldBe Nil
    List.concat(List(List(1, 2, 3), Nil, List(4, 5, 6), List(7, 8), Nil, List(9))) shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }
  "List.add_1" should "add 1 to all the elements of list" in {
    List.add_1(List(1, 2, 4)) shouldBe List(2, 3, 5)
    List.add_1(Nil) shouldBe Nil
  }
  "List.doubleToString" should "convert all the double elements of list to string" in {
    List.doubleToString(List(1.0, 2.0, 4.2)) shouldBe List("1.0", "2.0", "4.2")
    List.doubleToString(Nil) shouldBe Nil
  }
  "List.map" should "map all the elements of list" in {
    List.map(List(1.0, 2.0, 4.2))(_.toString) shouldBe List("1.0", "2.0", "4.2")
    List.map(List(1, 2, 4))(_ + 1) shouldBe List(2, 3, 5)
    List.map[Nothing, Nothing](Nil)(identity) shouldBe Nil
    List.map_1(List(1.0, 2.0, 4.2))(_.toString) shouldBe List("1.0", "2.0", "4.2")
    List.map_1(List(1, 2, 4))(_ + 1) shouldBe List(2, 3, 5)
    List.map_1[Nothing, Nothing](Nil)(identity) shouldBe Nil
  }
  "List.filter" should "filter elements of the list" in {
    List.filter(List(1.0, 2.0, 4.2))(_ % 2 == 0) shouldBe List(2.0)
    List.filter(List("Hi", "Hello"))(_.contains("llo")) shouldBe List("Hello")
    List.filterUsingFlatMap(List(1.0, 2.0, 4.2))(_ % 2 == 0) shouldBe List(2.0)
    List.filterUsingFlatMap(List("Hi", "Hello"))(_.contains("llo")) shouldBe List("Hello")
  }
  "List.flatMap" should "flat map all the elements" in {
    List.flatMap(List(1, 2, 4))(x => List(x, x)) shouldBe List(1, 1, 2, 2, 4, 4)
    List.flatMap_1(List(1, 2, 4))(x => List(x, x)) shouldBe List(1, 1, 2, 2, 4, 4)
    List.flatMap(List("Hi", "Hello"))(_.foldRight(Nil: List[Char]){ case (ch, l) => Cons(ch, l)}) shouldBe List('H', 'i', 'H', 'e', 'l', 'l', 'o')
    List.flatMap_1(List("Hi", "Hello"))(_.foldRight(Nil: List[Char]){ case (ch, l) => Cons(ch, l)}) shouldBe List('H', 'i', 'H', 'e', 'l', 'l', 'o')
  }
  "List.zip" should "zip two lists" in {
    List.zip(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1 -> 4, 2 -> 5, 3 -> 6)
    List.zip(List(1, 2, 3), List(4, 5, 6, 0)) shouldBe List(1 -> 4, 2 -> 5, 3 -> 6)
    List.zip(List(1, 2, 3, 0), List(4, 5, 6)) shouldBe List(1 -> 4, 2 -> 5, 3 -> 6)
    List.zip(Nil, List(4, 5, 6)) shouldBe Nil
  }
  "List.zipWith" should "zip two lists and apply given function" in {
    List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldBe List(5, 7, 9)
  }
  "List.hasSubsequence" should "zip two lists and apply given function" in {
    List.hasSubSequence(List(1, 2, 3), List(1)) shouldBe true
    List.hasSubSequence(List(1, 2, 3), List(1, 2)) shouldBe true
    List.hasSubSequence(List(1, 2, 3), List(2)) shouldBe true
    List.hasSubSequence(List(1, 2, 3), List(3)) shouldBe true
    List.hasSubSequence(List(1, 2, 3), List(2, 3)) shouldBe true
    List.hasSubSequence(List(1, 2, 3), List(1, 2, 3)) shouldBe true
    List.hasSubSequence(List(1, 2, 3), List(1, 3)) shouldBe false
    List.hasSubSequence(List(1, 2, 3), Nil) shouldBe true
    List.hasSubSequence(Nil, Nil) shouldBe true
  }
}
