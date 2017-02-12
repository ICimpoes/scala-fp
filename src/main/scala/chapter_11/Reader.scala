package chapter_11

case class Reader[R, A](run: R => A)

object Reader {

  def ask[R] = Reader[R, R](identity)

  implicit def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] =
      Reader(_ => a)

    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(st.run(r)).run(r))
  }

  implicit def lift[R, A](f: R => A): Reader[R, A] = Reader(f)

}