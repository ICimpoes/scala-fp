import chapter_10.Monoid._
import chapter_7.NonBlocking._

package object chapter_10 {

  def concatenate[A](as: Seq[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: Seq[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: Seq[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: Seq[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.size <= 2) {
      foldMap(v, m)(f)
    } else {
      val (fs, ss) = v.splitAt(v.size / 2)
      m.op(foldMapV(fs, m)(f), foldMapV(ss, m)(f))
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {

    override def op(a1: Par[A], a2: Par[A]): Par[A] =
      map2WithActor(a1, a2)(m.op)

    override def zero: Par[A] =
      unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(a => fork(unit(f(a))))


  def ordered(as: IndexedSeq[Int]): Boolean =
    foldMap(as, ascOrd)(Some(_)).isDefined

  val ascOrd = new Monoid[Option[Int]] {

    override def op(a1: Option[Int], a2: Option[Int]): Option[Int] =
      for {
        i1 <- a1
        i2 <- a2
        if i1 <= i2
      } yield i2

    override def zero: Option[Int] = Some(Int.MinValue)
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {

    override def op(a1: WC, a2: WC): WC = a1 -> a2 match {
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case (Stub(c1), Part(l, cnt, r)) => Part(c1 + l, cnt, r)
      case (Part(l, cnt, r), Stub(c2)) => Part(l, cnt, r + c2)
      case (Part(l1, cnt1, r1), Part(l2, cnt2, r2)) =>
        if (r1 + l2 nonEmpty) Part(l1, cnt1 + cnt2 + 1, r2)
        else Part(l1, cnt1 + cnt2, r2)

    }

    override def zero: WC = Stub("")

  }


  def wc(s: String): Int = {
    foldMapV(s, endoMonoid[WC]) { c => {
      case Stub(str) =>
        if (c == ' ')
          Part("", 0, str)
        else
          Stub(c + str)
      case Part(l, cnt, r) =>
        if (c == ' ')
          Part("", cnt + (if (l nonEmpty) 1 else 0), r)
        else
          Part(c + l, cnt, r)
    }
    }(wcMonoid.zero) match {
      case Part(l, c, r) =>
        c + (if (l nonEmpty) 1 else 0) + (if (r nonEmpty) 1 else 0)
      case Stub(str) =>
        if (str nonEmpty) 1 else 0
    }
  }


}
