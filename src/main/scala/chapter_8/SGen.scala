package chapter_8

import chapter_5.Stream

case class SGen[+A](forSize: Int => Gen[A])

object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(s => g.listOfN(Gen.unit(s)))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(s => {
      val nonEmptySize =
        if (s == 0) s + 1
        else s.abs
      g.listOfN(Gen.unit(nonEmptySize))
    })


  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      println(casesPerSize)
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map { i => g(i).forAll(f) }
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

}