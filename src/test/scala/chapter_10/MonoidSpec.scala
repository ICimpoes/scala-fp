package chapter_10

import org.scalatest.{FlatSpec, Matchers}

class MonoidSpec extends FlatSpec with Matchers {

  "Monoid.ordered" should "return correct value for asc ord" in {
    ordered(IndexedSeq(0, 1, 2, 3, 3, 4, 5, 5, 5)) shouldBe true
    ordered(IndexedSeq(1)) shouldBe true
    ordered(IndexedSeq()) shouldBe true
    ordered(IndexedSeq(1, 2, 3, 3, 4, 5, 1)) shouldBe false
    ordered(IndexedSeq(5, 4, 3, 2, 1)) shouldBe false
    ordered(IndexedSeq(Int.MinValue, Int.MinValue, Int.MinValue + 1, Int.MaxValue)) shouldBe true
  }
}
