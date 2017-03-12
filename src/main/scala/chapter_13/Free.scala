package chapter_13

import chapter_11.Monad
import chapter_13.Free.{FlatMap, Return}
import chapter_7.NonBlocking.Par

sealed trait Free[F[_], A] {
  self =>
  def flatMap[B](f: (A) => Free[F, B]): Free[F, B] =
    FlatMap(self, f)

  def map[B](f: A => B): Free[F, B] =
    FlatMap(self, f.andThen(Return(_)))
}

object Free {

  case class Return[F[_], A](a: A) extends Free[F, A]

  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

  case class FlatMap[F[_], A, B](s: Free[F, A],
                                 f: A => Free[F, B]) extends Free[F, B]

  type TR[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {

    override def flatMap[A, B](ma: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] =
      FlatMap(ma, f)

    override def unit[A](a: => A): Free[F, A] = Return(a)
  }


  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(v) =>
      v
    case Suspend(s) =>
      s()
    case FlatMap(free, f) => free match {
      case Return(v) =>
        runTrampoline(f(v))
      case Suspend(s) =>
        runTrampoline(f(s()))
      case FlatMap(free2, g) =>
        runTrampoline(free2.flatMap(fa2 => g(fa2).flatMap(f)))
    }
  }



}

object FreeApp extends App {


  import Free._


  val fM: TR[Int] = FlatMap(FlatMap(Return(5), (a: Int) => Return(a * 2)), (a: Int) => Return(a + 10))

  println(runTrampoline(fM))


}