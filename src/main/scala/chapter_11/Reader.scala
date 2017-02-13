package chapter_11

object Reader {

  type Reader[R, A] = Kleisli[Id, R, A]

  implicit def lift[R, A](f: R => A): Reader[R, A] = Kleisli(r => Id.idMonad.unit(f(r)))

}