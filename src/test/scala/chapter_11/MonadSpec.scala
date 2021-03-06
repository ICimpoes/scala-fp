package chapter_11

import chapter_4._
import org.scalatest.{FlatSpec, Matchers}
import chapter_11.Monad._
import chapter_12.Traverse.listTraverse

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

    Monad.optionMonad.composeUsingJoin(f, g)(-1) shouldBe None
    Monad.optionMonad.composeUsingJoin((_: Int) => None, g)(0) shouldBe None
    Monad.optionMonad.composeUsingJoin(f, g)(0) shouldBe Some("i = 0")
  }
  "Monad.join" should "correctly join Option[Option[A]]" in {
    Monad.optionMonad.join(Some(None)) shouldBe None
    Monad.optionMonad.join(Some(Some(1))) shouldBe Some(1)
    Monad.optionMonad.join(None) shouldBe None
  }
  "Monad.zipWithIndex" should "zip a list with index using State Monad" in {
    Monad.zipWithIndex(List("1", "2", "3")) shouldBe List(0 -> "1", 1 -> "2", 2 -> "3")
    Monad.zipWithIndex(Nil) shouldBe Nil
  }
  "Monad.composeM" should "compose two monads" in {
    Monad.composeM(optionMonad, listMonad, listTraverse).map(Some(List(1, 2)))(i => (i + 1).toString) shouldBe Some(List("2", "3"))
    Monad.composeM(optionMonad, listMonad, listTraverse).map(Some(Nil))(identity) shouldBe Some(Nil)
    Monad.composeM(optionMonad, listMonad, listTraverse).map(None)(identity) shouldBe None
  }
  "Monad.OptionT" should "flatMap over M[Option[_]]" in {
    OptionT[({type f[x] = Either[String, x]})#f, Int](Right(Some(12)))(eitherMonad[String]).map(_.toString).value shouldBe Right(Some("12"))
    OptionT[({type f[x] = Either[String, x]})#f, Int](Right(None))(eitherMonad[String]).map(_.toString).value shouldBe Right(None)
    OptionT[({type f[x] = Either[String, x]})#f, Int](Left("12"))(eitherMonad[String]).map(_.toString).value shouldBe Left("12")
  }
}
