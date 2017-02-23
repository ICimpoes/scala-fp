package chapter_12

import chapter_10.Monoid.Const
import chapter_10.{Foldable, Monoid}
import chapter_11.{Functor, Id, Monad}
import chapter_3.{Branch, Leaf, Tree}
import chapter_6.State
import chapter_6.State._

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))


  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(identity)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => Id(f(a))).a

  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M, x]})#f, A, Nothing](
      as)(f)(Monoid.monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) => for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i)).run(0)._1

  override def toList[A](fa: F[A]): List[A] =
    traverseS(fa) { (a: A) =>
      for {
        l <- get[List[A]]
        _ <- set(a :: l)
      } yield ()
    }.run(Nil)._2.reverse

  def toList_[A](fa: F[A]): List[A] =
    mapAccum[A, List[A], Unit](fa)((a, l) => ((), a :: l))(Nil)._2.reverse

  def zipWithIndex_[A](ta: F[A]): F[(A, Int)] =
    mapAccum[A, Int, (A, Int)](ta)((a, i: Int) => (a -> i) -> (i + 1))(0)._1

  def mapAccum[A, B, R](fa: F[A])(f: (A, B) => (R, B))(z: B): (F[R], B) =
    traverseS(fa) { (a: A) =>
      for {
        b <- get[B]
        (r, b1) = f(a, b)
        _ <- set(b1)
      } yield r
    }.run(z)

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
      fa.fold(Applicative[G] unit (None: Option[B]))(a => Applicative[G].map(f(a))(Some(_)))
  }

  val treeTraverse = new Traverse[Tree] {
    override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa.map(f)

    override def traverse[G[_] : Applicative, A, B](fa: Tree[A])(f: (A) => G[B]): G[Tree[B]] =
      fa.fold(a => Applicative[G].map(f(a))(Leaf(_): Tree[B]))((gb1, gb2) => Applicative[G].map2(gb1, gb2)((t1, t2) => Branch(t1, t2): Tree[B]))
  }

}