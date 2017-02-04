package chapter_10

import chapter_3.{Branch, Leaf}
import org.scalatest.{FlatSpec, Matchers}

class TreeFoldableSpec extends FlatSpec with Matchers {

  val tree = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Branch(Branch(Leaf(4), Leaf(5)), Leaf(6)))
  val leaf = Leaf(10)

  "TreeFoldable.foldLeft" should "return correct value" in {
    Foldable.FoldableTree.foldLeft(tree)(1)(_ - _) shouldBe -20
    Foldable.FoldableTree.foldLeft(leaf)(1)(_ - _) shouldBe -9
  }
  "TreeFoldable.foldRight" should "return correct value" in {
    Foldable.FoldableTree.foldRight(tree)(1)(_ - _) shouldBe -2
    Foldable.FoldableTree.foldRight(leaf)(1)(_ - _) shouldBe 9
  }
  "TreeFoldable.foldMap" should "return correct value" in {
    Foldable.FoldableTree.foldMap(tree)(_ - 2)(Monoid.intAddition) shouldBe 9
    Foldable.FoldableTree.foldMap(leaf)(_ - 2)(Monoid.intAddition) shouldBe 8
  }
  "TreeFoldable.toList" should "return correct value" in {
    Foldable.FoldableTree.toList(tree) shouldBe List(1, 2, 3, 4, 5, 6)
    Foldable.FoldableTree.toList(leaf) shouldBe List(10)
  }

}
