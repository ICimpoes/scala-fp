package chapter_10

import chapter_10.Monoid._
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
  "productMonoid" should "return product of two monoids" in {
    foldMap(List("1", "2", "3"), productMonoid(stringMonoid, intMultiplication))(s => s -> s.toInt) shouldBe "123" -> 6
  }
  "mergeMonoid" should "merge nested maps" in {
    val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))
    val map1 = Map("k1" -> Map("1" -> 1, "2" -> 2), "k2" -> Map("1" -> 10, "3" -> 30))
    val map2 = Map("k1" -> Map("2" -> 2), "k3" -> Map("1" -> 100))
    M.op(map1, map2) should contain theSameElementsAs Map(
      "k1" -> Map("1" -> 1, "2" -> 4),
      "k2" -> Map("1" -> 10, "3" -> 30),
      "k3" -> Map("1" -> 100))
  }

  "bag" should "create a bag out of Seq" in {
    val seq = IndexedSeq("a", "b", "c", "a", 1, 2, 1, "c", "a")
    bag(seq) shouldBe Map("a" -> 3, "b" -> 1, "c" -> 2, 1 -> 2, 2 -> 1)
  }
}
