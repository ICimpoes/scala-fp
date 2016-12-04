package chapter_3

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  "Tree.size" should "return correct size of a tree" in {
    Tree.size(Leaf(1)) shouldBe 1
    Tree.size(Branch(Leaf(1), Leaf(1))) shouldBe 3
    Tree.size(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))) shouldBe 5
  }
  "Tree.sizeUsingFold" should "return correct size of a tree" in {
    Tree.sizeUsingFold(Leaf(1)) shouldBe 1
    Tree.sizeUsingFold(Branch(Leaf(1), Leaf(1))) shouldBe 3
    Tree.sizeUsingFold(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))) shouldBe 5
  }

  "Tree.maximum" should "return maximum value of a tree" in {
    Tree.maximum(Leaf(1)) shouldBe 1
    Tree.maximum(Branch(Leaf(1), Leaf(2))) shouldBe 2
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
    Tree.maximum(Branch(Branch(Leaf(3), Leaf(2)), Leaf(1))) shouldBe 3
    Tree.maximum(Branch(Branch(Leaf(2), Leaf(3)), Leaf(1))) shouldBe 3
  }

  "Tree.maximumUsingFold" should "return maximum value of a tree" in {
    Tree.maximumUsingFold(Leaf(1)) shouldBe 1
    Tree.maximumUsingFold(Branch(Leaf(1), Leaf(2))) shouldBe 2
    Tree.maximumUsingFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
    Tree.maximumUsingFold(Branch(Branch(Leaf(3), Leaf(2)), Leaf(1))) shouldBe 3
    Tree.maximumUsingFold(Branch(Branch(Leaf(2), Leaf(3)), Leaf(1))) shouldBe 3
  }

  "Tree.depth" should "return depth of a tree" in {
    Tree.depth(Leaf(1)) shouldBe 0
    Tree.depth(Branch(Leaf(1), Leaf(2))) shouldBe 1
    Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 2
    Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 2
    Tree.depth(Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(2), Leaf(3)))) shouldBe 2
    Tree.depth(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Branch(Leaf(2), Leaf(3)))) shouldBe 3
  }

  "Tree.depthUsingFold" should "return depth of a tree" in {
    Tree.depthUsingFold(Leaf(1)) shouldBe 0
    Tree.depthUsingFold(Branch(Leaf(1), Leaf(2))) shouldBe 1
    Tree.depthUsingFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 2
    Tree.depthUsingFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 2
    Tree.depthUsingFold(Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(2), Leaf(3)))) shouldBe 2
    Tree.depthUsingFold(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Branch(Leaf(2), Leaf(3)))) shouldBe 3
  }

  "Tree.map" should "map all the elements of a tree" in {
    Tree.map(Leaf(1))(_ + 1) shouldBe Leaf(2)
    Tree.map(Branch(Leaf(1), Leaf(2)))(_ + 3) shouldBe Branch(Leaf(4), Leaf(5))
    Tree.map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_ - 5) shouldBe Branch(Branch(Leaf(-4), Leaf(-3)), Leaf(-2))
  }

  "Tree.mapUsingFold" should "map all the elements of a tree" in {
    Tree.mapUsingFold(Leaf(1))(_ + 1) shouldBe Leaf(2)
    Tree.mapUsingFold(Branch(Leaf(1), Leaf(2)))(_ + 3) shouldBe Branch(Leaf(4), Leaf(5))
    Tree.mapUsingFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_ - 5) shouldBe Branch(Branch(Leaf(-4), Leaf(-3)), Leaf(-2))
  }

}
