package chapter_13

sealed trait IO2[A] {
  def flatMap[B](f: A => IO2[B]): IO2[B] =
    FlatMap(this, f)

  def map[B](f: A => B): IO2[B] =
    flatMap(f andThen (Return(_)))
}

case class Return[A](a: A) extends IO2[A]

case class Suspend[A](resume: () => A) extends IO2[A]

case class FlatMap[A, B](sub: IO2[A], k: A => IO2[B]) extends IO2[B]