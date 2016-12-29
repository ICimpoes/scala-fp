package chapter_8

import chapter_6.{RNG, SimpleRNG}
import chapter_8.Prop._

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop((ms, tc, rng) => {
    val result1 = run(ms, tc, rng)
    if (!result1.isFalsified)
      p.run(ms, tc, rng)
    else
      result1
  })

  def ||(p: Prop): Prop = Prop((ms, tc, rng) => {
    val result1 = run(ms, tc, rng)
    if (result1.isFalsified)
      p.run(ms, tc, rng)
    else
      result1
  })

  def exec(maxSize: Int = 100,
           testCases: Int = 100,
           rng: RNG = SimpleRNG(System.currentTimeMillis)): Result =
    Prop.run(this, maxSize, testCases, rng)

}

object Prop {


  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Result =
    p.run(maxSize, testCases, rng) match {
      case f@Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
        f
      case p@Passed =>
        println(s"+ OK, passed $testCases tests.")
        p
    }

  type FailedCase = String

  type SuccessCount = Int

  type TestCases = Int

  type MaxSize = Int

}