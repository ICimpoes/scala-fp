package chapter_5


sealed trait Stream[+A] {

  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }


  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) =>
      val hh = h()
      val tt = t()
      hh :: tt.toList
  }

  def take(n: Int): Stream[A] = {
    if (n > 0) {
      this match {
        case Empty => Empty
        case Cons(h, t) =>
          val hh = h()
          val tt = t()
          cons(hh, tt.take(n - 1))
      }
    } else {
      Empty
    }
  }

  def takeWhile(f: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) =>
        val hh = h()
        if (f(hh)) {
          val tt = t()
          cons(hh, tt.takeWhile(f))
        }
        else Empty
    }
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileUsingFR(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else empty)

  def headOptionUsingFR: Option[A] =
    foldRight(None: Option[A])((a, b) => Option(a))

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}