package chapter_12

import chapter_3.{Branch, Leaf, Tree}
import org.scalatest.{FlatSpec, Matchers}


class TraverseSpec extends FlatSpec with Matchers {

  import chapter_12.Applicative.optionApplicative

  "Traverse.listTraverse" should "traverse List[Option] to Option[List]" in {

    Traverse.listTraverse.traverse[Option, Int, String](List(1, 2, 3))(i => Some(i.toString)) shouldBe Some(List("1", "2", "3"))
    Traverse.listTraverse.traverse[Option, Int, String](List(1, 2, 3))(i => if (i % 2 == 0) Some(i.toString) else None) shouldBe None
    Traverse.listTraverse.traverse[Option, Int, String](List())(i => if (i % 2 == 0) Some(i.toString) else None) shouldBe Some(Nil)

  }
  "Traverse.treeTraverse" should "traverse List[Option] to Option[List]" in {

    import chapter_12.Applicative.optionApplicative

    val tree: Tree[Int] = Branch(Branch(Leaf(2), Leaf(3)), Leaf(1))

    Traverse.treeTraverse.traverse[Option, Int, String](tree)(i => Some(i.toString)) shouldBe Some(tree.map(_.toString))
    Traverse.treeTraverse.traverse[Option, Int, String](tree)(i => if (i % 2 == 0) Some(i.toString) else None) shouldBe None
    Traverse.treeTraverse.traverse[Option, Int, String](Leaf(1))(i => if (i % 2 == 0) Some(i.toString) else None) shouldBe None
    Traverse.treeTraverse.traverse[Option, Int, String](Leaf(1))(i => Some(i.toString)) shouldBe Some(Leaf("1"))

  }

}
