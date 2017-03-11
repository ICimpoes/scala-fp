package chapter_13

import chapter_11.Monad
import chapter_11.MonadOps._
import chapter_13.TailRec._

import scala.annotation.tailrec

sealed trait TailRec[A] { self =>

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

  def flatMap[B](f: A => TailRec[B]): TailRec[B] =
    FlatMap(this, f)

  def map[B](f: A => B): TailRec[B] =
    flatMap(f andThen (Return(_)))
}

object TailRec {

  implicit val TailRecMonad = new Monad[TailRec] {
    def unit[A](a: => A): TailRec[A] = Return(a)

    def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]) = fa flatMap f
  }

  case class Return[A](a: A) extends TailRec[A]

  case class Suspend[A](resume: () => A) extends TailRec[A]

  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  def printLine(s: String): TailRec[Unit] =
    Suspend(() => Return(println(s)))

}


object TailRecApp extends App {

  import TailRec._

  forever(printLine("Still going...")).run
}