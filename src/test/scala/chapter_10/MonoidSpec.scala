package chapter_10

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class MonoidSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  val wcTest = Table(("string", "word count"),
    ("", 0),
    ("      ", 0),
    ("11 2 1 2", 4),
    ("   111  2   1   2   ", 4),
    ("1111 1111  11111  21111   11111   11 12   ", 7),
    ("1111 1111  11111  21111   11111   11 12   1", 8)
  )

  "Monoid.ordered" should "return correct value for asc ord" in {
    ordered(IndexedSeq(0, 1, 2, 3, 3, 4, 5, 5, 5)) shouldBe true
    ordered(IndexedSeq(1)) shouldBe true
    ordered(IndexedSeq()) shouldBe true
    ordered(IndexedSeq(1, 2, 3, 3, 4, 5, 1)) shouldBe false
    ordered(IndexedSeq(5, 4, 3, 2, 1)) shouldBe false
    ordered(IndexedSeq(Int.MinValue, Int.MinValue, Int.MinValue + 1, Int.MaxValue)) shouldBe true
  }

  "Monoid.wc" should "return correct word count" in {
    forAll(wcTest) { (in, res) =>
      wc(in) shouldBe res
    }
  }

  "Monoid.wc2" should "return correct word count" in {
    forAll(wcTest) { (in, res) =>
      wc2(in) shouldBe res
    }
  }
  "Monoid.wc3" should "return correct word count" in {
    forAll(wcTest) { (in, res) =>
      wc3(in) shouldBe res
    }
  }
}
