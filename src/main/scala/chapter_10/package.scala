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

}
