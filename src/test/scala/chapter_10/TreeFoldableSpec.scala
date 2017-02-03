package chapter_10

import chapter_3.{Branch, Leaf}
import org.scalatest.{FlatSpec, Matchers}

class TreeFoldableSpec extends FlatSpec with Matchers {

  val tree = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Branch(Branch(Leaf(4), Leaf(5)), Leaf(6)))
  val leaf = Leaf(10)

  "TreeFoldable.foldLeft" should "return correct value" in {
    Foldable.foldableTree.foldLeft(tree)(1)(_ - _) shouldBe -20
    Foldable.foldableTree.foldLeft(leaf)(1)(_ - _) shouldBe -9
  }
  "TreeFoldable.foldRight" should "return correct value" in {
    Foldable.foldableTree.foldRight(tree)(1)(_ - _) shouldBe -2
    Foldable.foldableTree.foldRight(leaf)(1)(_ - _) shouldBe 9
  }
  "TreeFoldable.foldMap" should "return correct value" in {
    Foldable.foldableTree.foldMap(tree)(_ - 2)(Monoid.intAddition) shouldBe 9
    Foldable.foldableTree.foldMap(leaf)(_ - 2)(Monoid.intAddition) shouldBe 8
  }

}
