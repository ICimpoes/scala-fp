package chapter_8

import chapter_8.Prop.{FailedCase, SuccessCount}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  val isFalsified = false
}

case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  val isFalsified = true
}