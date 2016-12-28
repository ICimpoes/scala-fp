package chapter_8

import chapter_6.{RNG, State}
import chapter_5.Stream

case class Gen[+A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(s => Gen(State.sequence(List.fill(s)(sample))))

  def forAll(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      randomStream(rng)
        .zipWith(Stream.from(0))
        .take(n)
        .map { case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(Gen.buildMsg(a, e), i)
          }
        }
        .find(_.isFalsified)
        .getOrElse(Passed)
  }

  def randomStream(rng: RNG): Stream[A] =
    Stream.unfold(rng)(r => Some(sample.run(r)))

  def unsized: SGen[A] = SGen(_ => this)

}


object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State.nonNegativeLessThan(stopExclusive - start).map(_ + start))

  def unit[A](a: => A): Gen[A] =
    Gen(State[RNG, A](a -> _))

  def boolean: Gen[Boolean] =
    Gen(State.int.map(_ % 2 == 0))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, w1) = g1
    val (gen2, w2) = g2
    val thrshG1 = w1.abs / (w1.abs + w2.abs)
    Gen(State.double).flatMap(d => if (d < thrshG1) gen1 else gen2)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

}
