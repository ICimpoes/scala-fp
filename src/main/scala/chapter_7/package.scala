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
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(sumPar(l), sumPar(r))(_ + _)
    }
  }

}
