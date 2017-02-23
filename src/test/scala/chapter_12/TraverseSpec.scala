package chapter_12

import chapter_3.{Branch, Leaf, Tree}
import org.scalatest.{FlatSpec, Matchers}


class TraverseSpec extends FlatSpec with Matchers {

  val tree: Tree[Int] = Branch(Branch(Leaf(2), Leaf(3)), Leaf(1))

  import chapter_12.Applicative.optionApplicative

  "Traverse.listTraverse" should "traverse List[Option] to Option[List]" in {

    Traverse.listTraverse.traverse[Option, Int, String](List(1, 2, 3))(i => Some(i.toString)) shouldBe Some(List("1", "2", "3"))
    Traverse.listTraverse.traverse[Option, Int, String](List(1, 2, 3))(i => if (i % 2 == 0) Some(i.toString) else None) shouldBe None
    Traverse.listTraverse.traverse[Option, Int, String](List())(i => if (i % 2 == 0) Some(i.toString) else None) shouldBe Some(Nil)

  }
  "Traverse.treeTraverse" should "traverse List[Option] to Option[List]" in {

    import chapter_12.Applicative.optionApplicative

    Traverse.treeTraverse.traverse[Option, Int, String](tree)(i => Some(i.toString)) shouldBe Some(tree.map(_.toString))
    Traverse.treeTraverse.traverse[Option, Int, String](tree)(i => if (i % 2 == 0) Some(i.toString) else None) shouldBe None
    Traverse.treeTraverse.traverse[Option, Int, String](Leaf(1))(i => if (i % 2 == 0) Some(i.toString) else None) shouldBe None
    Traverse.treeTraverse.traverse[Option, Int, String](Leaf(1))(i => Some(i.toString)) shouldBe Some(Leaf("1"))

  }
  "Traverse.map" should "map over option" in {
    Traverse.optionTraverse.map(Some(1))(_.toString) shouldBe Some("1")
    Traverse.optionTraverse.map(None)(identity) shouldBe None
  }
  "Traverse.listTraverse" should "correctly zip list with index" in {
    Traverse.listTraverse.zipWithIndex(List(1, 2, 3)) shouldBe List(1, 2, 3).zipWithIndex
    Traverse.listTraverse.zipWithIndex(Nil) shouldBe Nil

    Traverse.listTraverse.zipWithIndex_(List(1, 2, 3)) shouldBe List(1, 2, 3).zipWithIndex
    Traverse.listTraverse.zipWithIndex_(Nil) shouldBe Nil
  }
  "Traverse.toList" should "correctly convert a traversable to list" in {
    Traverse.optionTraverse.toList(Some(1)) shouldBe List(1)
    Traverse.optionTraverse.toList(None) shouldBe Nil
    Traverse.treeTraverse.toList(tree) shouldBe List(2, 3, 1)
    Traverse.treeTraverse.toList(Leaf(1)) shouldBe List(1)

    Traverse.optionTraverse.toList_(Some(1)) shouldBe List(1)
    Traverse.optionTraverse.toList_(None) shouldBe Nil
    Traverse.treeTraverse.toList_(tree) shouldBe List(2, 3, 1)
    Traverse.treeTraverse.toList_(Leaf(1)) shouldBe List(1)
  }
}
