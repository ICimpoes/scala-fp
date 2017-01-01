package chapter_8

import java.util.concurrent.Executors

import chapter_5.Stream
import chapter_6.{RNG, State}
import chapter_7.Par._

case class Gen[+A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](gB: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(gB.sample)(f))

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

  def forAllPar(f: A => Par[Boolean]): Prop =
    Gen.S ** this forAll { case (s, a) =>
      f(a)(s).get()
    }

  def randomStream(rng: RNG): Stream[A] =
    Stream.unfold(rng)(r => Some(sample.run(r)))

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g) ((_, _))

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

  val S = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25)

  def checkPar(p: => Par[Boolean]): Prop =
    S.forAll(p(_).get())

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

}
