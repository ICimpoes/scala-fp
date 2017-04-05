package chapter_15

import chapter_15.Process._

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

//  def |>[O2](p2: Process[O, O2]): Process[I, O2] = this -> p2 match {
//    case (Halt(), _) => Halt()
//    case (_, Halt()) => Halt()
//    case (a, Emit(o, t2)) => emit[I, O2](o, a |> t2)
//    case (Await(fi), a) => Await[I, O2](o => fi(o) |> a)
//    case (Emit(i, t), Await(fo)) => t |> fo(Some(i))
//    case (_, Await(fo)) => this |> fo(None)
//  }

}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]

case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]

object Process {

  def await[I, O](f: I => Process[I, O], fallback: Process[I, O] = Halt[I, O]()) = Await[I, O] {
    case Some(i) => f(i)
    case _ => fallback
  }

  def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Emit[I, O] = Emit[I, O](head, tail)

  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit[I, O](f(i))
      case None => Halt()
    }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] =
    await[I, I](i => if (p(i)) emit(i) else Halt()).repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      await(d => emit(d + acc, go(d + acc)))
    go(0.0)
  }

  def take[I](n: Int): Process[I, I] = {
    if (n <= 0) Halt()
    else await(a => emit(a, take(n - 1)))
  }

  def drop[I](n: Int): Process[I, I] = {
    await { a =>
      if (n <= 0) emit(a, drop(n))
      else drop(n - 1)
    }
  }

  def takeWhile[I](f: I => Boolean): Process[I, I] = {
    await { a =>
      if (f(a)) emit(a, takeWhile(f))
      else Halt()
    }
  }

  def dropWhile[I](f: I => Boolean): Process[I, I] = {
    await { a =>
      if (!f(a)) emit(a, dropWhile(_ => false))
      else dropWhile(f)
    }
  }

  def count[I]: Process[I, Int] = {
    def go(cnt: Int): Process[I, Int] =
      await(d => emit(cnt, go(cnt + 1)), Emit(cnt))
    go(0)
  }

  def mean: Process[Double, Double] = {
    def go(cnt: Int, sum: Double): Process[Double, Double] =
      await(d => emit((sum + d) / (cnt + 1), go(cnt + 1, sum + d)))
    go(0, 0)
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    await { i =>
      val (o, s2) = f(i, z)
      emit(o, loop(s2)(f))
    }

  def sum_2: Process[Double, Double] =
    loop(0.0)((x, acc) => (x + acc, x + acc))

  def count_2[I]: Process[I, Int] =
    loop(0)((_, acc) => (acc + 1, acc + 1))

//  def count_3[I]: Process[I, Int] =
//    lift[I, Double](_ => 0.0) |> sum |> lift(_.toInt)

}