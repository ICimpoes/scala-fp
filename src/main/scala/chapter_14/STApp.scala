package chapter_14

object STApp extends App {

//  val p = new RunnableST[(Int, Int)] {
//    def apply[S] = for {
//      r1 <- STRef(1)
//      r2 <- STRef(2)
//      x <- r1.read
//      y <- r2.read
//      _ <- r1.write(y + 1)
//      _ <- r2.write(x + 1)
//      a <- r1.read
//      b <- r2.read
//    } yield (a, b)
//  }
//
//  println(ST.runST(p))
//
//
//
//  ST.runST(new RunnableST[Unit] {
//    override def apply[S]: ST[S, Unit] = for {
//      a <- STArray(10, "1")
//      a1 <- a.fill(Map(1 -> "2", 2 -> "3", 3 -> "4", 4 -> "5"))
//      x0 <- a.read(0)
//      x1 <- a.read(1)
//      x2 <- a.read(2)
//      x3 <- a.read(3)
//      x4 <- a.read(4)
//      x5 <- a.read(5)
//      x6 <- a.read(6)
//    } yield println(s"$x0 $x1 $x2 $x3 $x4 $x5 $x6")
//  })

  ST.runST(new RunnableST[List[Int]] {
    override def apply[S]: ST[S, List[Int]] = {
      for {
        arr <- STArray.fromList[S, Int](List(2, 5, 3, 1, 6))
        _ <- STArray.partition(arr, 0, 4, 2)
        l <- arr.freeze
      } yield l
    }
  }).foreach(println)

}