import chapter_7.Par.Par
import chapter_7.Par._

package object chapter_7 {

  def sum(l: IndexedSeq[Int]): Int = {
    if (l.size <= 1)
      l.headOption getOrElse 0
    else {
      val (f, s) = l.splitAt(l.size / 2)
      sum(f) + sum(s)
    }
  }

  def sumPar(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      fork(sumPar(l)).map2(fork(sumPar(r)))(_ + _)
    }
  }

  def sortPar(ps: Par[List[Int]]): Par[List[Int]] =
    ps.map(_.sorted)


}
