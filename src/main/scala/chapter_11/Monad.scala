package chapter_11

import chapter_12.{Applicative, Traverse}

trait Monad[F[_]] extends Applicative[F] {

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
   flatMap(fa)((a: A) => unit(f(a)))

  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def flatMapUsingCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)()

  def flatMapUsingJoin[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def replicateM1[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(Nil)
    else map2(ma, replicateM1(n - 1, ma))(_ :: _)

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List.empty[A]))(
      (a, fList) =>
        compose(f, if (_: Boolean) map(fList)(a :: _) else fList)(a)
    )

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def composeUsingJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)

}

object Monad {

  import chapter_4._
  import chapter_5.Stream
  import chapter_6.State
  import chapter_6.State._
  import chapter_7.Par
  import chapter_7.Par.Par
  import chapter_8.Gen
  import chapter_9.MyParser.{Parser, parser}

  def composeM[F[_], G[_]](implicit F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] = new Monad[({type f[x] = F[G[x]]})#f] {

    override def unit[A](a: => A): F[G[A]] =
      F.unit(G.unit(a))

    override def flatMap[A, B](ma: F[G[A]])(f: A => F[G[B]]) =
      F.flatMap(ma)(ga => F.map(T.traverse(ga)(f))(G.join))
  }



  implicit val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  implicit val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  implicit val parserMonad = new Monad[Parser] {
    override def unit[A](a: => A): Parser[A] = parser.succeed(a)

    override def flatMap[A, B](ma: Parser[A])(f: (A) => Parser[B]): Parser[B] = parser.flatMap(ma)(f)
  }

  implicit val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma.flatMap(f)
  }

  implicit val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  implicit val listMonad = new Monad[scala.List] {
    override def unit[A](a: => A): scala.List[A] = scala.List(a)

    override def flatMap[A, B](ma: scala.List[A])(f: (A) => scala.List[B]): scala.List[B] = ma.flatMap(f)
  }

  implicit def stateMonad[S] = new Monad[({type L[?] = State[S, ?]})#L] {
    override def unit[A](a: => A): State[S, A] = State(a -> _)

    override def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma.flatMap(f)
  }

  implicit def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](fa: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = fa.flatMap(f)
  }

  def zipWithIndex[A](as: scala.List[A]): scala.List[(Int, A)] =
    as.foldLeft(stateMonad[Int].unit(scala.List[(Int, A)]()))((acc, a) => for {
      xs <- acc
      n <- get
      _ <- set(n + 1)
    } yield (n, a) :: xs).run(0)._1.reverse


  case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Monad[M]) {

    def map[B](f: A => B): OptionT[M, B] =
      flatMap(a => OptionT(M.unit(Some(f(a)))))

    def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] =
      OptionT(M.flatMap(value) {
        case None => M.unit(None)
        case Some(a) => f(a).value
      })

  }

  trait Ops[F[_], A] {
    def self: F[A]

    def flatMap[B](f: A => F[B])(implicit ev: Monad[F]): F[B] = ev.flatMap(self)(f)

    def map[B](f: A => B)(implicit ev: Monad[F]): F[B] = ev.map(self)(f)

    def **[B](fb: F[B])(implicit ev: Monad[F]): F[(A, B)] = ev.product(self, fb)
  }

  object Ops {
    implicit def toAllMonad[F[_], A](target: F[A])(implicit ev: Monad[F]) = new Ops[F, A] {
      override def self = target
    }
  }

}

final case class Kleisli[F[_], A, B](run: A => F[B]) { self =>

  def apply(a: A): F[B] = run(a)

  def map[C](f: B => C)(implicit F: Monad[F]): Kleisli[F, A, C] =
    Kleisli(a => F.map(run(a))(f))

  def flatMap[C](f: B => Kleisli[F, A, C])(implicit F: Monad[F]): Kleisli[F, A, C] =
    Kleisli((r: A) => F.flatMap[B, C](run(r))((b: B) => f(b).run(r)))

  def compose[Z](f: Z => F[A])(implicit F: Monad[F]): Kleisli[F, Z, B] =
    Kleisli((z: Z) => F.flatMap(f(z))(run))
}

object Kleisli {
  implicit def lift[F[_], A, B](run: A => F[B]) = Kleisli(run)
}