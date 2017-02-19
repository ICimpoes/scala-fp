package chapter_12

import org.scalatest.{FlatSpec, Matchers}


class TraverseSpec extends FlatSpec with Matchers {

  "Traverse.listTraverse" should "traverse List[Option] to Option[List]" in {

    import chapter_12.Applicative.optionApplicative

    Traverse.listTraverse.traverse[Option, Int, String](List(1, 2, 3))(i => Some(i.toString)) shouldBe Some(List("1", "2", "3"))
    Traverse.listTraverse.traverse[Option, Int, String](List(1, 2, 3))(i => if (i % 2 == 0) Some(i.toString) else None) shouldBe None
    Traverse.listTraverse.traverse[Option, Int, String](List())(i => if (i % 2 == 0) Some(i.toString) else None) shouldBe Some(Nil)

  }

}
