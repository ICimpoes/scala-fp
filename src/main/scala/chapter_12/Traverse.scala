package chapter_12

import chapter_10.Monoid.Const
import chapter_10.{Foldable, Monoid}
import chapter_11.{Functor, Id}
import chapter_3.{Branch, Leaf, Tree}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(identity)

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => Id(f(a))).a

  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M, x]})#f, A, Nothing](
      as)(f)(Monoid.monoidApplicative(mb))
}

object Traverse {

  val listTraverse = new Traverse[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa.map(f)

    override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: (A) => G[B]): G[List[B]] =
      fa.foldRight(Applicative[G].unit(List.empty[B]))((a, fbs) => Applicative[G].map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {

    override def map[A, B](fa: Option[A])(f: (A) => B) = fa.map(f)

    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: (A) => G[B]): G[Option[B]] =
      fa.fold(Applicative[G]unit(None: Option[B]))(a => Applicative[G].map(f(a))(Some(_)))
  }

  val treeTraverse = new Traverse[Tree] {
    override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa.map(f)

    override def traverse[G[_] : Applicative, A, B](fa: Tree[A])(f: (A) => G[B]): G[Tree[B]] =
      fa.fold(a => Applicative[G].map(f(a))(Leaf(_): Tree[B]))((gb1, gb2) => Applicative[G].map2(gb1, gb2)((t1, t2) => Branch(t1, t2): Tree[B]))
  }

}