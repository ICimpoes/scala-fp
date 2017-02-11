package chapter_11

case class Id[A](a: A) {
  def map[B](f: A => B) = Id(f(a))

  def flatMap[B](f: A => Id[B]): Id[B] = f(a)
}

object Id {

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma.flatMap(f)
  }

}