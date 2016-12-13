package chapter_6


import java.lang.Integer.{MAX_VALUE, MIN_VALUE}

import chapter_6.RNG._
import org.scalatest.{FlatSpec, Matchers}

class RNGSpec extends FlatSpec with Matchers {

  val maxRng = TestRNG(MAX_VALUE)
  val minRng = TestRNG(MIN_VALUE)
  val zeroRng = TestRNG(0)
  val sRng: Rand[String] = map(int)(x => s"int: $x")

  "RNG.nextInt" should "return next int and new RNG state" in {
    val (i, rng) = int(TestRNG(1))
    i shouldBe 1
    rng shouldBe TestRNG(2)
    rng.nextInt shouldBe (2 -> TestRNG(3))
  }

  "RNG.double" should "return double value" in {
    double(zeroRng)._1 shouldBe 0
    double(maxRng)._1 shouldBe MAX_VALUE.toDouble / MAX_VALUE
    double(minRng)._1 shouldBe MIN_VALUE.toDouble / (MAX_VALUE + 1)
  }

  "RNG.ints" should "return list of random ints" in {
    val (l, r) = ints(10)(zeroRng)
    val (l2, r2) = ints2(10)(zeroRng)

    l shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    r shouldBe TestRNG(10)

    l2 shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    r2 shouldBe TestRNG(10)
  }

  "RNG.unit" should "return Rand unit" in {
    unit("aa")(zeroRng) shouldBe "aa" -> zeroRng
  }

  "RNG.map" should "map over RNG" in {
    val (x1, r1) = sRng(zeroRng)
    val (x2, r2) = sRng(r1)
    val (x3, r3) = sRng(r2)
    val (x4, r4) = sRng(r3)

    x1 shouldBe "int: 0"
    x2 shouldBe "int: 1"
    x3 shouldBe "int: 2"
    x4 shouldBe "int: 3"
    r4 shouldBe TestRNG(4)
  }

  "RNG.map2" should "map over two RNG" in {
    val iSRng = map2(int, sRng)((i, s) => s"$s - $i")
    val (x1, r1) = iSRng(zeroRng)
    val (x2, r2) = iSRng(r1)
    val (x3, r3) = iSRng(r2)
    val (x4, r4) = iSRng(r3)

    x1 shouldBe "int: 1 - 0"
    x2 shouldBe "int: 3 - 2"
    x3 shouldBe "int: 5 - 4"
    x4 shouldBe "int: 7 - 6"
    r4 shouldBe TestRNG(8)
  }

  "RNG.both" should "return tuple of 2 RNG" in {
    val iSRng = both(int, sRng)
    val (x1, r1) = iSRng(zeroRng)
    val (x2, r2) = iSRng(r1)
    val (x3, r3) = iSRng(r2)
    val (x4, r4) = iSRng(r3)

    x1 shouldBe 0 -> "int: 1"
    x2 shouldBe 2 -> "int: 3"
    x3 shouldBe 4 -> "int: 5"
    x4 shouldBe 6 -> "int: 7"
    r4 shouldBe TestRNG(8)
  }

  "RNG.sequence" should "return tuple of 2 RNG" in {
    val (x1, r1) = int(zeroRng)
    val (x2, r2) = int(r1)
    val (x3, r3) = int(r2)
    val (x4, r4) = int(r3)

    val (l1 , rng1) = sequence(List(int, int, int, int))(zeroRng)
    val (l2 , rng2) = sequence2(List(int, int, int, int))(zeroRng)


    l1 shouldBe List(x1, x2, x3, x4)
    rng1 shouldBe r4

    l2 shouldBe List(x1, x2, x3, x4)
    rng2 shouldBe r4
  }

  "RNG.nonNegativeEven" should "return non negative and even ints" in {
    val (x1, r1) = nonNegativeEven(minRng)
    val (x2, r2) = nonNegativeEven(r1)
    val (x3, r3) = nonNegativeEven(r2)

    x1 shouldBe MAX_VALUE - 1
    x2 shouldBe MAX_VALUE - 1
    x3 shouldBe MAX_VALUE - 3
    r3 shouldBe TestRNG(MIN_VALUE + 3)
  }
}
