package chapter_7

import java.util.concurrent.Executors

import chapter_7.Par._
import chapter_8._
import org.scalatest.{FlatSpec, Matchers}

class ParSpec extends FlatSpec with Matchers {

  val es = Executors.newCachedThreadPool

  "Par.unit" should "work correctly" in {
    val res = Gen.unit(Par.unit(1)).forAll(i =>
      i.map(_ + 1)(es).get == Par.unit(2)(es).get)

    res.exec() shouldBe Passed
    Prop.check(
      Par.unit(1).map(_ + 1).equal(Par.unit(2))(es).get
    ).exec() shouldBe Proved

    Gen.checkPar(Par.unit(1).map(_ + 1).equal(Par.unit(2))).exec() shouldBe Passed
  }

  "Par.map" should "work correctly" in {
    val pint = Gen.choose(0,10) map Par.unit
    val p = pint.forAllPar(n => Par.map(n)(y => y).equal(n))

    p.exec() shouldBe Passed

  }

  "Par.fork" should "work correctly" in {
    val pint: Gen[Par[Int]] = Gen.choose(0,10) map unit
    val p = pint.forAllPar(n => n.equal(fork(n)))

    p.exec() shouldBe Passed
  }
}
