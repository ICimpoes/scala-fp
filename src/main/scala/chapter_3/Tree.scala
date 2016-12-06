package chapter_3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size(tree: Tree[_]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) =>
      1 + size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l).max(maximum(r))
  }


  def depth(tree: Tree[_]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeUsingFold(tree: Tree[_]): Int =
    fold(tree)(_ => 1)(1 + _ + _)

  def maximumUsingFold(tree: Tree[Int]): Int =
    fold(tree)(identity)(_ max _)

  def depthUsingFold(tree: Tree[_]): Int =
    fold(tree)(_ => 0)((l, r) => 1 + (l max r))

  def mapUsingFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))

}