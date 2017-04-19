package chapter_15.extensible

import chapter_15.extensible.Process._

object Process1 {

  case class Is[I]() {

    sealed trait f[X]

    val Get = new f[I] {}
  }

  def Get[I] = Is[I]().Get

  type Process1[I, O] = Process[Is[I]#f, O]

  def await1[I, O](recv: I => Process1[I, O], fallback: Process1[I, O] = halt1[I, O]): Process1[I, O] =
    Await(Get[I], (e: Either[Throwable, I]) => e match {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(i) => Try(recv(i))
    })

  def emit1[I, O](h: O, tl: Process1[I, O] = halt1[I, O]): Process1[I, O] =
    emit(h, tl)

  def halt1[I, O]: Process1[I, O] = Halt[Is[I]#f, O](End)


  def lift[I, O](f: I => O): Process1[I, O] =
    await1[I, O](i => emit(f(i))) repeat

  def filter[I](f: I => Boolean): Process1[I, I] =
    await1[I, I](i => if (f(i)) emit(i) else halt1) repeat

  def id[I]: Process1[I, I] =
    await1[I, I](i => emit1(i, id))

}
