package chapter_10

import chapter_3.{Branch, Leaf, Tree}
import chapter_4.{Option, None, Some}

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] =
    foldRight(fa)(List.empty[A])(_ :: _)
}

object Foldable {

  object FoldableList extends Foldable[List] {

    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  }

  object FoldableIndSeq extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  }


  object FoldableStream extends Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  }

  object FoldableTree extends Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

    override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B = as.fold(f)(mb.op)
  }

  object FoldableOption extends Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case Some(a) => f(a, z)
      case _ => z
    }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
      case Some(a) => f(z, a)
      case _ => z
    }

    override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
      case Some(a) => f(a)
      case None => mb.zero
    }
  }
}