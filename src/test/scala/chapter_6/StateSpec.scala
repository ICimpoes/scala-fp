package chapter_6

import chapter_6.State._
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers {

  "State.set" should "set State" in {
    val rngState = for {
      i <- int
      s <- set[RNG](TestRNG(6))
      y <- int
    } yield i -> y

    val ((i1, y1), s1) = rngState.run(TestRNG(0))
    i1 shouldBe 0
    y1 shouldBe 6
    s1 shouldBe TestRNG(7)

    val ((i2, y2), s2) = rngState.run(s1)
    i2 shouldBe 7
    y2 shouldBe 6
    s2 shouldBe TestRNG(7)
  }

  "State.get" should "get current State" in {
    val rngState = for {
      i <- int
      s <- get
      y <- int
    } yield (i, y, s)

    val ((i1, y1, st1), s1) = rngState.run(TestRNG(0))
    i1 shouldBe 0
    st1 shouldBe TestRNG(1)
    y1 shouldBe 1
    s1 shouldBe TestRNG(2)

    val ((i2, y2, st2), s2) = rngState.run(s1)
    i2 shouldBe 2
    st2 shouldBe TestRNG(3)
    y2 shouldBe 3
    s2 shouldBe TestRNG(4)
  }

  "State.modify" should "modify current State" in {
    val rngState = for {
      i <- int
      _ <- modify[RNG](_ => TestRNG(i * 2))
      y <- int
    } yield (i, y)

    val ((i1, y1), s1) = rngState.run(TestRNG(3))
    i1 shouldBe 3
    y1 shouldBe 6
    s1 shouldBe TestRNG(7)

    val ((i2, y2), s2) = rngState.run(s1)
    i2 shouldBe 7
    y2 shouldBe 14
    s2 shouldBe TestRNG(15)
  }

}
