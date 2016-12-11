package chapter_6

trait RNG {

  def nextInt: (Int, RNG)

}

object RNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val res@(i, r) = rng.nextInt
    if (i < 0) -(i + 1) -> r
    else res
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    i.toDouble / Integer.MAX_VALUE -> r
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (p, r) = intDouble(rng)
    p.swap -> r
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    (d1, d2, d3) -> r3
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(cnt: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (cnt > 0) {
        val (i, r) = rng.nextInt
        go(cnt - 1, r, acc.+:(i))
      } else {
        acc -> rng
      }
    }
    go(count, rng, Nil)
  }

}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
