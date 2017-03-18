package chapter_13

import chapter_11.Monad
import chapter_13.ConsoleState.Buffers

case class ConsoleState[A](run: Buffers => (A, Buffers)) {
  def map[B](f: A => B): ConsoleState[B] =
    ConsoleState { b =>
      val (a, buff) = run(b)
      f(a) -> buff
    }

  def flatMap[B](f: A => ConsoleState[B]): ConsoleState[B] =
    ConsoleState{ b =>
      val (a, buff1) = run(b)
      f(a).run(buff1)
    }
}

object ConsoleState {
  case class Buffers(in: List[String], out: List[String])

  implicit val monad: Monad[ConsoleState] = new Monad[ConsoleState] {
    def flatMap[A, B](ma: ConsoleState[A])(f: (A) => ConsoleState[B]): ConsoleState[B] = ma.flatMap(f)
    def unit[A](a: => A): ConsoleState[A] = ConsoleState(a -> _)
  }

}
