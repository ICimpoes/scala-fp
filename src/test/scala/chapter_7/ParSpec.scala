package chapter_7

import java.util.concurrent.Executors

import chapter_7.Par._
import chapter_8.{Gen, Passed}
import org.scalatest.{FlatSpec, Matchers}

class ParSpec extends FlatSpec with Matchers {

  val es = Executors.newCachedThreadPool

  "Par.unit" should "work correctly" in {
    val res = Gen.unit(Par.unit(1)).forAll(i =>
      i.map(_ + 1)(es).get == Par.unit(2)(es).get)

    res.exec() shouldBe Passed
  }
}
