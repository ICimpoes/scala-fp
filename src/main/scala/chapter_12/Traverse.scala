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
    mapAccum[A, List[A], Unit](fa)(Nil)((a, l) => ((), a :: l))._2.reverse

  def zipWithIndex_[A](ta: F[A]): F[(A, Int)] =
    mapAccum[A, Int, (A, Int)](ta)(0)((a, i: Int) => (a -> i) -> (i + 1))._1

  def mapAccum[A, B, R](fa: F[A])(z: B)(f: (A, B) => (R, B)): (F[R], B) =
    traverseS(fa) { (a: A) =>
      for {
        b <- get[B]
        (r, b1) = f(a, b)
        _ <- set(b1)
      } yield r
    }.run(z)

  def reverse[A](fa: F[A]): F[A] =
    mapAccum[A, List[A], A](fa)(toList(fa).reverse)( (_, b) => (b.head, b.tail))._1

  def foldLeft_[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum[A, B, Unit](as)(z)((a, b) => () -> f(b, a))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa)(toList(fb)) {
      case (a, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) ={
    implicit val productF = G.productF(H)
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => productF.map(f(a) -> g(a))(identity))
  }

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