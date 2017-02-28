package chapter_12

import chapter_12.Applicative._
import org.scalatest.{FlatSpec, Matchers}

class ApplicativeSpec extends FlatSpec with Matchers {

  "Applicative.productF" should "create product of two applicatives" in {
    def product[E] = optionApplicative.productF[({type f[x] = Either[E, x]})#f](eitherApplicative)
    product.map(Some(2) -> Right(12))(_ + 1) shouldBe (Some(3), Right(13))
    product.map(None -> Right(12))(_ + 1) shouldBe (None, Right(13))
    product.map(None -> Left[String, Int]("12"))(_ + 1) shouldBe (None, Left("12"))

  }
  "Applicative.compose" should "compose two applicatives" in {
    def composed[E] = optionApplicative.compose[({type f[x] = Either[E, x]})#f](eitherApplicative)
    composed.map(Some(Right(12)))(_ + 1) shouldBe Some(Right(13))
    composed.map[Int, Int](None)(_ + 1) shouldBe None
    composed.map[Int, Int](Some(Left[String, Int]("1")))(_ + 1) shouldBe Some(Left[String, Int]("1"))
  }

}
