package chapter_12

import chapter_11.Functor

trait Applicative[F[_]] extends Functor[F] {
  self =>

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply[A, B](unit(f))(fa)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_ (_))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A],
                          fb: F[B],
                          fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  //(A => B => C)(A) = B => C
  //(B => C)(B) = C
  def map2_[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply[B, C](map(fa)(f.curried))(fb)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(unit(Map.empty[K, V])) { case (fa, (k, fv)) => map2(fa, fv)((a, v) => a + (k -> v)) }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)(_ -> _)

  def productF[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({type f[x] = (F[x], G[x])})#f] {

    override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
      self.map2(fa._1, fb._1)(f) -> G.map2(fa._2, fb._2)(f)

    override def unit[A](a: => A): (F[A], G[A]) = self.unit(a) -> G.unit(a)

  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {

      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
    }

}

object Applicative {

  def apply[F[_]: Applicative]: Applicative[F] = implicitly[Applicative[F]]

  implicit val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    def map2[A, B, C](a: Stream[A], b: Stream[B])(
      f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  implicit val optionApplicative = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      for {
        a <- fa
        b <- fb
      } yield f(a, b)
  }

  implicit def eitherApplicative[E] = new Applicative[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def map2[A, B, C](fa: Either[E, A], fb: Either[E, B])(f: (A, B) => C): Either[E, C] =
      for {
        a <- fa
        b <- fb
      } yield f(a, b)
  }

}
