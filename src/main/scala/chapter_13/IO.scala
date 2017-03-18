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
