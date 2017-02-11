package chapter_11

import org.scalatest.{FlatSpec, Matchers}

class IdSpec extends FlatSpec with Matchers {

  "Id.map" should "map id value" in {
    Id("hi").map(_.toUpperCase) shouldBe Id("HI")
  }
  "Id.flatMap" should "flatMap over id value" in {
    Id("hi").flatMap(a => Id(s"$a, man")) shouldBe Id("hi, man")
    Id("hi").flatMap(a => Id(a.length)) shouldBe Id(2)
  }

}
