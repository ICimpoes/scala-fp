package chapter_8

import chapter_6.{RNG, State}

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(s => Gen(State.sequence(List.fill(s)(sample))))


}


object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State.nonNegativeLessThan(stopExclusive - start).map(_ + start))

  def unit[A](a: => A): Gen[A] =
    Gen(State[RNG, A](a -> _))

  def boolean: Gen[Boolean] =
    Gen(State.int.map(_ % 2 == 0))

}
