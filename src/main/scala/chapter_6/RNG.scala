package chapter_6

trait RNG {

  def nextInt: (Int, RNG)

}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  val double: Rand[Double] =
    map(nonNegativeInt)(_ / (Integer.MAX_VALUE.toDouble + 1))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val res@(i, r) = rng.nextInt
    if (i < 0) -(i + 1) -> r
    else res
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (p, r) = intDouble(rng)
    p.swap -> r
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
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

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)(_ -> _)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldRight(List[A]() -> rng) {
        case (ra, (l, r1)) =>
          val (e, r2) = ra(r1)
          (e :: l) -> r2
      }
    }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)


}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
