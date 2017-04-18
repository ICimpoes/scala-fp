package chapter_15.extensible

case class T[I, I2]() {

  sealed trait f[X] {
    def get: Either[I => X, I2 => X]
  }

  val L = new f[I] {
    def get = Left(identity)
  }

  val R = new f[I2] {
    def get = Right(identity)
  }
}

object T {

  import chapter_15.extensible.Process._

  def L[I, I2] = T[I, I2]().L

  def R[I, I2] = T[I, I2]().R

  type Tee[I, I2, O] = Process[T[I, I2]#f, O]

  def haltT[I, I2, O]: Tee[I, I2, O] =
    Halt[T[I, I2]#f, O](End)

  def awaitL[I, I2, O](recv: I => Tee[I, I2, O], fallback: => Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] =
    await[T[I, I2]#f, I, O](L) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a) => Try(recv(a))
    }

  def awaitR[I, I2, O](recv: I2 => Tee[I, I2, O], fallback: => Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] =
    await[T[I, I2]#f, I2, O](R) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a) => Try(recv(a))
    }

  def emitT[I, I2, O](h: O, tl: Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] =
    emit(h, tl)

  def zipWith[I, I2, O](f: (I, I2) => O): Tee[I, I2, O] =
    awaitL[I, I2, O](i => awaitR(i2 => emitT(f(i, i2)))) repeat

  def zip[I, I2]: Tee[I, I2, (I, I2)] = zipWith((_, _))

}