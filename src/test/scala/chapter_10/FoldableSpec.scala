package chapter_10

import chapter_10.Foldable.FoldableOption
import chapter_4._
import org.scalatest.{FlatSpec, Matchers}

class FoldableSpec extends FlatSpec with Matchers {

  "FoldableOption.toLIst" should "work" in {
    FoldableOption.toList(Some(1)) shouldBe List(1)
    FoldableOption.toList(None) shouldBe Nil
  }

}
