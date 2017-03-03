package chapter_13

import chapter_11.Monad

sealed trait IO[A] {
  self =>

  def run: A

  def map[B](f: A => B): IO[B] =
    new IO[B] {
      def run = f(self.run)
    }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {
      def run = f(self.run).run
    }

}

object IO {

  import chapter_11.Monad.Ops._

  def apply[A](r: => A) = new IO[A] {
    def run: A = r
  }

  implicit val IOMonad = new Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] {
      def run = a
    }

    def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

    def apply[A](a: => A): IO[A] = unit(a)
  }

  def doWhile[A, F[_]](a: F[A])(cond: A => F[Boolean])(implicit ev: Monad[F]): F[Unit] = for {
    a1 <- a
    ok <- cond(a1)
    _ <- if (ok) doWhile(a)(cond) else ev.unit(())
  } yield ()

  def when[A, F[_]](cond: Boolean)(f: => F[A])(implicit ev: Monad[F]): F[Boolean] = {
    if (cond) f
    ev.unit(cond)
  }

  def sequence_[A, F[_]](fs: F[A]*)(implicit ev: Monad[F]): F[Unit] = foreachM(fs.toStream)(skip(_))

  def forever[A, F[_]](a: F[A])(implicit ev: Monad[F]): F[A] = {
    lazy val t: F[A] = forever(a)
    a flatMap (_ => t)
  }

  def foldM[A, B, F[_]](l: Stream[A])(z: B)(f: (B, A) => F[B])(implicit ev: Monad[F]): F[B] =
    l match {
      case h #:: t => f(z, h) flatMap (z2 => foldM(t)(z2)(f))
      case _ => ev.unit(z)
    }

  def skip[A, F[_]](fa: F[A])(implicit ev: Monad[F]): F[Unit] = fa.map(_ => ())

  def foldM_[A, B, F[_]](l: Stream[A])(z: B)(f: (B, A) => F[B])(implicit ev: Monad[F]): F[Unit] =
    skip { foldM(l)(z)(f) }


  def foreachM[A, F[_]](l: Stream[A])(f: A => F[Unit])(implicit ev: Monad[F]): F[Unit] =
    foldM_(l)(())((u, a) => skip(f(a)))


  def PrintLine(line: Any) = IO(println(line))

  val ReadLine: IO[String] = IO(io.StdIn.readLine())

  val empty: IO[Unit] = IO[Unit](())

  case class Ref[A](var a: A) { self =>
    def modify(f: A => A) = {
      a = f(a)
      IO(self)
    }
    def set[B](b: B) = IO(Ref(b))
    def get = IO(a)
  }

  def ref[A](value: A) = IO[Ref[A]](Ref(value))

  val echo = ReadLine.flatMap(PrintLine)

  val readInt = ReadLine.map(_.toInt)

  val readInts: IO[(Int, Int)] = readInt ** readInt

}
