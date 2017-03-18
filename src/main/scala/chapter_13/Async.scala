package chapter_13

import java.util.concurrent.Executors

import chapter_11.Monad
import chapter_11.MonadOps._
import chapter_13.Async._
import chapter_7.Par.Par
import chapter_7.Par


sealed trait Async[A] {
  def flatMap[B](f: A => Async[B]): Async[B] =
    FlatMap(this, f)

  def map[B](f: A => B): Async[B] =
    flatMap(f andThen (Return(_)))
}

object Async {

  case class Return[A](a: A) extends Async[A]

  case class Suspend[A](resume: Par[A]) extends Async[A]

  case class FlatMap[A, B](sub: Async[A],
                           k: A => Async[B]) extends Async[B]

  @annotation.tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a) => Par.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }

  implicit val AsyncMonad = new Monad[Async] {
    def unit[A](a: => A): Async[A] = Return(a)

    def flatMap[A, B](fa: Async[A])(f: A => Async[B]) = fa flatMap f
  }

  def printLine(s: String): Async[Unit] =
    Suspend(Par.lazyUnit(println(s)))

}


object AsyncRecApp extends App {

  import Async._

  val es = Executors.newCachedThreadPool

  Stream.fill(100)("bla")

  run(foreachM(Stream.fill(100)("bla")){l => Thread.sleep(1000);printLine(l)})(es)

  run(printLine("a"))
}