package chapter_8

import chapter_8.SGen._
import org.scalatest.{FlatSpec, Matchers}

class PropSpec extends FlatSpec with Matchers {

  val smallInt = Gen.choose(-10, 10)

  "Prop.run" should "return Passed for forAll returning true" in {
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp) shouldBe Passed
  }

  "Prop.run" should "return Falsified for forAll returning false" in {
    val failSizeProp = forAll(listOf(smallInt))(_.size < 3)

    Prop.run(failSizeProp) shouldBe a[Falsified]
  }
  "Prop for list.sort" should "return Passed" in {
    val sortProp = forAll(listOf1(smallInt)){ ns =>
      val sorted = ns.sorted
      !sorted.tails.exists(l => l.exists(_ < l.head ))
    }

    Prop.run(sortProp) shouldBe Passed
  }
}
