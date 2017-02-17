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

}
