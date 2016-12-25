package chapter_8

trait Prop {

  self =>

  import Prop._

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop): Prop = new Prop {
    val check = for {
      s1 <- self.check
      s2 <- p.check
    } yield s1 + s2
  }

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}