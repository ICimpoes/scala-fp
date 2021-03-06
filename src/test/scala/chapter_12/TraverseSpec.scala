package chapter_12

import chapter_3.{Branch, Leaf, Tree}
import org.scalatest.{FlatSpec, Matchers}


class TraverseSpec extends FlatSpec with Matchers {

  val tree: Tree[Int] = Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(1), Leaf(4)))

  import chapter_12.Applicative.optionApplicative
  import chapter_11.Monad.listMonad

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
    Traverse.treeTraverse.toList(tree) shouldBe List(2, 3, 1, 4)
    Traverse.treeTraverse.toList(Leaf(1)) shouldBe List(1)

    Traverse.optionTraverse.toList_(Some(1)) shouldBe List(1)
    Traverse.optionTraverse.toList_(None) shouldBe Nil
    Traverse.treeTraverse.toList_(tree) shouldBe List(2, 3, 1, 4)
    Traverse.treeTraverse.toList_(Leaf(1)) shouldBe List(1)
  }
  "Traverse.reverse" should "correctly reverse a traversable" in {
    Traverse.listTraverse.reverse(List(1, 2, 3, 4, 5, 6)) shouldBe List(6, 5, 4, 3, 2, 1)
    Traverse.optionTraverse.reverse(Some(1)) shouldBe Some(1)
    Traverse.optionTraverse.reverse(None) shouldBe None
    Traverse.treeTraverse.reverse(tree) shouldBe Branch(Branch(Leaf(4), Leaf(1)), Branch(Leaf(3), Leaf(2)))
    Traverse.treeTraverse.reverse(Branch(Branch(Leaf(4), Leaf(1)), Leaf(3))) shouldBe Branch(Branch(Leaf(3), Leaf(1)), Leaf(4))
  }
  "Traverse.foldLeft_" should "correctly fold a traversable" in {
    Traverse.listTraverse.foldLeft_(List(1, 2, 3, 4, 5))("Start: ")((s, i) => s"$s, $i") shouldBe "Start: , 1, 2, 3, 4, 5"
  }
  "Traverse.zip" should "correctly zip a traversable" in {
    Traverse.treeTraverse.zip(tree, tree) shouldBe tree.map(i => i -> i)
  }
  "Traverse.fuse" should "correctly fuse a traversable" in {
    Traverse.treeTraverse.fuse[Option, List, Int, String](tree)(a => Option(a.toString), a => List(a.toString)) shouldBe (Some(tree.map(_.toString)), List(tree.map(_.toString)))
  }
}
