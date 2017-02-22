package chapter_10

import chapter_10.Foldable.FoldableOption
import chapter_4._
import org.scalatest.{FlatSpec, Matchers}

class FoldableSpec extends FlatSpec with Matchers {

  val foldList = new Foldable[List] {
    override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = {
      if (as.isEmpty) mb.zero
      else {
        mb.op(f(as.head), foldMap(as.tail)(f)(mb))
      }
    }
  }

  "FoldableOption.toLIst" should "work" in {
    FoldableOption.toList(Some(1)) shouldBe List(1)
    FoldableOption.toList(None) shouldBe Nil
  }

  "FoldableOption.foldLeft" should "work being implemented via foldMap" in {
    foldList.foldMap(List(1, 2, 3, 4))(_.toString)(Monoid.stringMonoid) shouldBe "1234"
    foldList.foldRight(List(1, 2, 3, 4, 5))("Start: ")((s, i) => s"$s, $i") shouldBe "1, 2, 3, 4, 5, Start: "
    foldList.foldRight(List(1, 2, 3, 4))(2)(_ - _) shouldBe 0
    foldList.foldLeft(List(1, 2, 3, 4, 5))("Start: ")((s, i) => s"$s, $i") shouldBe "Start: , 1, 2, 3, 4, 5"
  }

}
