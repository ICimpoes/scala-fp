package chapter_8

import chapter_6.RNG
import chapter_8.Prop._

case class Prop(run: (TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop((tc, rng) => {
    val result1 = run(tc, rng)
    if (result1.isFalsified)
      p.run(tc, rng)
    else
      result1
  })

  def ||(p: Prop): Prop = Prop((tc, rng) => {
    val result1 = run(tc, rng)
    if (!result1.isFalsified)
      p.run(tc, rng)
    else
      result1

  })

}

object Prop {

  type FailedCase = String

  type SuccessCount = Int

  type TestCases = Int

}