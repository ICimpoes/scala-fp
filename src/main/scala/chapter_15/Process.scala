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

  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit[I, O](f(i))
      case None => Halt()
    }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i) if p(i) => Emit(i)
      case _ => Halt()
    }.repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None => Halt()
      }
    go(0.0)
  }

  def take[I](n: Int): Process[I, I] = {
    if (n <= 0) Halt()
    else
      Await {
        case Some(a) => Emit(a, take(n - 1))
        case None => Halt()
      }
  }

  def drop[I](n: Int): Process[I, I] = {
    Await {
      case Some(a) if n <= 0 => Emit(a, drop(n))
      case Some(_) => drop(n - 1)
      case None => Halt()
    }
  }

  def takeWhile[I](f: I => Boolean): Process[I, I] = {
    Await {
      case Some(a) if f(a) => Emit(a, takeWhile(f))
      case _ => Halt()
    }
  }

  def dropWhile[I](f: I => Boolean): Process[I, I] = {
    Await {
      case Some(a) if !f(a) => Emit(a, dropWhile(_ => false))
      case Some(_) => dropWhile(f)
      case _ => Halt()
    }
  }

}