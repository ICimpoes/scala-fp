package chapter_13

import chapter_11.Monad
import chapter_11.MonadOps._
import chapter_13.IO2._

import scala.annotation.tailrec

sealed trait IO2[A] { self =>

  @tailrec
  final def run: A = self match {
    case Return(a) =>
      a
    case Suspend(resume) =>
      resume()
    case FlatMap(x, f) => x match {
      case Return(a) =>
        f(a).run
      case Suspend(resume) =>
        f(resume()).run
      case FlatMap(x2, f2) =>
        x2.flatMap(f2).flatMap(f).run
    }

  }

  def flatMap[B](f: A => IO2[B]): IO2[B] =
    FlatMap(this, f)

  def map[B](f: A => B): IO2[B] =
    flatMap(f andThen (Return(_)))
}

object IO2 {

  implicit val IO2Monad = new Monad[IO2] {
    def unit[A](a: => A): IO2[A] = Return(a)

    def flatMap[A, B](fa: IO2[A])(f: A => IO2[B]) = fa flatMap f
  }

  case class Return[A](a: A) extends IO2[A]

  case class Suspend[A](resume: () => A) extends IO2[A]

  case class FlatMap[A, B](sub: IO2[A], k: A => IO2[B]) extends IO2[B]

  def printLine(s: String): IO2[Unit] =
    Suspend(() => Return(println(s)))

}


object IO2App extends App {

  import IO2._

  forever(printLine("Still going...")).run
}