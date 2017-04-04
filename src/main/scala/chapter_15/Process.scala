package chapter_15

sealed trait Process[I, O] {

  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }

}

case class Emit[I, O](
                       head: O,
                       tail: Process[I, O] = Halt[I, O]())
  extends Process[I, O]

case class Await[I, O](
                        recv: Option[I] => Process[I, O])
  extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]

object Process {

  def await[I, O](f: I => Process[I, O], fallback: Process[I, O] = Halt[I, O]()) = Await[I, O] {
    case Some(i) => f(i)
    case _ => fallback
  }

  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit[I, O](f(i))
      case None => Halt()
    }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] =
    await[I, I](i => if (p(i)) Emit(i) else Halt()).repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      await(d => Emit(d + acc, go(d + acc)))
    go(0.0)
  }

  def take[I](n: Int): Process[I, I] = {
    if (n <= 0) Halt()
    else await(a => Emit(a, take(n - 1)))
  }

  def drop[I](n: Int): Process[I, I] = {
    await { a =>
      if (n <= 0) Emit(a, drop(n))
      else drop(n - 1)
    }
  }

  def takeWhile[I](f: I => Boolean): Process[I, I] = {
    await { a =>
      if (f(a)) Emit(a, takeWhile(f))
      else Halt()
    }
  }

  def dropWhile[I](f: I => Boolean): Process[I, I] = {
    await { a =>
      if (!f(a)) Emit(a, dropWhile(_ => false))
      else dropWhile(f)
    }
  }

  def count[I]: Process[I, Int] = {
    def go(cnt: Int): Process[I, Int] =
      await(d => Emit(cnt, go(cnt + 1)), Emit(cnt))
    go(0)
  }

  def mean: Process[Double, Double] = {
    def go(cnt: Int, sum: Double): Process[Double, Double] =
      await(d => Emit((sum + d) / (cnt + 1), go(cnt + 1, sum + d)))
    go(0, 0)
  }


}