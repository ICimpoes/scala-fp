package chapter_11

import chapter_4._
import org.scalatest.{FlatSpec, Matchers}

class MonadSpec extends FlatSpec with Matchers {

  "Monad.sequence" should "transform List[Option] into Option[List]" in {
    Monad.optionMonad.sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    Monad.optionMonad.sequence(List(Some(1), Some(2), None)) shouldBe None
    Monad.optionMonad.sequence(List(Some(1), None, Some(3))) shouldBe None
    Monad.optionMonad.sequence(List(None, Some(2), Some(3))) shouldBe None
  }

  "Monad.traverse" should "transform List[Option] into Option[List]" in {
    Monad.optionMonad.traverse(List(1, 2, 3))(Some(_)) shouldBe Some(List(1, 2, 3))
    Monad.optionMonad.traverse(List(1, 2, 3))(i => if (i == 1) None else Some(i)) shouldBe None
    Monad.optionMonad.traverse(List(1, 2, 3))(i => if (i == 2) None else Some(i)) shouldBe None
    Monad.optionMonad.traverse(List(1, 2, 3))(i => if (i == 3) None else Some(i)) shouldBe None
  }

  "Monad.filterM" should "filter List using A => F[Boolean]" in {
    Monad.optionMonad.filterM(List(1, 2, 3, 4))(i => if (i <= 2) Some(true) else Some(false)) shouldBe Some(List(1, 2))
    Monad.optionMonad.filterM(List(1, 2, 3, 4))(i => if (i <= 2) Some(true) else None) shouldBe None
  }
  "Monad.compose" should "compose Int => Option[Int] and Int => Option[String]" in {
    val f: Int => Option[Int] = Some(_)
    def g(i: Int) = {
      if (i >= 0) Some(s"i = $i")
      else None
    }
    Monad.optionMonad.compose(f, g)(-1) shouldBe None
    Monad.optionMonad.compose((_: Int) => None, g)(0) shouldBe None
    Monad.optionMonad.compose(f, g)(0) shouldBe Some("i = 0")
  }

}
