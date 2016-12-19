package chapter_7

case class Par[A](a: () => A)

object Par {

  def unit[A](a : => A): Par[A] = ???

  def get[A](p: Par[A]): A = ???

  def map2[A, B, C](pA: Par[A], pB: Par[B])(f: (A, B) => C): Par[C] = ???

}
