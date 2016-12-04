package chapter_4

import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  val n: Option[Int] = None

  val s: Option[Int] = Some(1)

  "Option.map" should "apply the given function on option" in {
    n.map(_ + 1) shouldBe None
    s.map(_ + 1) shouldBe Some(2)
  }
  "Option.flatMap" should "apply the given function on option" in {
    n.flatMap(x => Some(x + 1)) shouldBe None
    s.flatMap(x => Some(x + 1)) shouldBe Some(2)
  }
  "Option.getOrElse" should "return default on None" in {
    n.getOrElse(2) shouldBe 2
    s.getOrElse(2) shouldBe 1
  }
  "Option.orElse" should "return default on None" in {
    n.orElse(Some(2)) shouldBe Some(2)
    n.orElse(None) shouldBe None
    s.orElse(Some(2)) shouldBe Some(1)
    s.orElse(None) shouldBe Some(1)
  }
  "Option.filter" should "correctly filter optional value" in {
    n.filter(_ > 1) shouldBe None
    s.filter(_ > 0) shouldBe Some(1)
    s.filter(_ > 1) shouldBe None
  }
  "map2" should "map over two options" in {
    map2(n, s)(_ + _) shouldBe None
    map2(s, n)(_ + _) shouldBe None
    map2(n, n)(_ + _) shouldBe None
    map2(s, s)(_ + _) shouldBe Some(2)
  }
  "sequence" should "return option of list" in {
    sequence(Nil) shouldBe Some(Nil)
    sequence(List(n, s)) shouldBe None
    sequence(List(s, s)) shouldBe Some(List(1, 1))
    sequence(List(s, Some(2))) shouldBe Some(List(1, 2))
    sequenceUsingTraverse(Nil) shouldBe Some(Nil)
    sequenceUsingTraverse(List(n, s)) shouldBe None
    sequenceUsingTraverse(List(s, s)) shouldBe Some(List(1, 1))
    sequenceUsingTraverse(List(s, Some(2))) shouldBe Some(List(1, 2))
  }
  "traverse" should "return option of list" in {
    traverse(Nil)(_ => None) shouldBe Some(Nil)
    traverse(List(1, 2, 3))(Some(_)) shouldBe Some(List(1, 2, 3))
    traverse(List(1, 2, 3))(x => if (x == 2) None else Some(x)) shouldBe None
  }

}
