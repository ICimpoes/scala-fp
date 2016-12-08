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

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 =>
      cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def takeUsingUnfold(n: Int): Stream[A] =
    unfold(this -> n) {
      case (Cons(h, t), i) if i > 0 =>
        Some(h() -> (t() -> (i - 1)))
      case _ => None
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

  def takeWhileUsingUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) =>
        val hh = h()
        if (f(hh)) Some(hh -> t())
        else None
      case _ => None
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
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else empty)

  def headOptionUsingFR: Option[A] =
    foldRight(None: Option[A])((a, b) => Option(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def mapUsingUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()) -> t())
      case _ => None
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[AA >: A](as: Stream[AA]): Stream[AA] =
    foldRight(as)(cons[AA](_, _))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def zipWith[B](bs: Stream[B]): Stream[(A, B)] =
    unfold(this -> bs) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(h1() -> h2(), t1() -> t2())
      case _ => None
    }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this -> bs) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(Some(h1()) -> Some(h2()), t1() -> t2())
      case (Cons(h, t), Empty) =>
        Some(Some(h()) -> None, t() -> Empty)
      case (_, Cons(h, t)) =>
        Some(None -> Some(h()), Empty -> t())
      case _ => None
    }
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

  def constant[A](elem: A) = {
    lazy val s: Stream[A] = cons(elem, s)
    s
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs(n: Int = 0, m: Int = 1): Stream[Int] =
    cons(n, fibs(m, n + m))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) =>
        cons(a, unfold(s)(f))
      case None =>
        empty[A]
    }

  def constantUsingUnfold[A](elem: A): Stream[A] =
    unfold(elem)(_ => Some(elem -> elem))

  def fromUsingUnfold(n: Int): Stream[Int] =
    unfold(n)(i => Some(i -> (i + 1)))

  def fibsUsingUnfold(): Stream[Int] =
    unfold(0 -> 1){case (f, s) => Some(f, s -> (f + s))}

}