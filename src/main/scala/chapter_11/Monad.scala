package chapter_11

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def flatMapUsingCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)()

  def flatMapUsingJoin[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List.empty[A]))((fa, list) => map2(fa, list)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, list) => map2(f(a), list)(_ :: _))

  def replicateM1[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(Nil)
    else map2(ma, replicateM1(n - 1, ma))(_ :: _)

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

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

  import chapter_3.List
  import chapter_4._
  import chapter_5.Stream
  import chapter_6.State
  import chapter_6.State._
  import chapter_7.Par
  import chapter_7.Par.Par
  import chapter_8.Gen
  import chapter_9.MyParser.{Parser, parser}


  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  val parserMonad = new Monad[Parser] {
    override def unit[A](a: => A): Parser[A] = parser.succeed(a)

    override def flatMap[A, B](ma: Parser[A])(f: (A) => Parser[B]): Parser[B] = parser.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = List.flatMap(ma)(f)
  }

  def stateMonad[S] = new Monad[({type L[?] = State[S, ?]})#L] {
    override def unit[A](a: => A): State[S, A] = State(a -> _)

    override def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma.flatMap(f)
  }

  def zipWithIndex[A](as: scala.List[A]): scala.List[(Int, A)] =
    as.foldLeft(stateMonad[Int].unit(scala.List[(Int, A)]()))((acc, a) => for {
      xs <- acc
      n <- get
      _ <- set(n + 1)
    } yield (n, a) :: xs).run(0)._1.reverse

  trait Ops[F[_], A] {
    def self: F[A]

    def flatMap[B](f: A => F[B])(implicit ev: Monad[F]): F[B] = ev.flatMap(self)(f)
  }

  object Ops {
    implicit def toAllMonad[F[_], A](target: F[A])(implicit ev: Monad[F]) = new Ops[F, A] {
      override def self = target
    }
  }

}