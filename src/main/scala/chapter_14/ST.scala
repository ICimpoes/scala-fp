package chapter_14

import scala.reflect.ClassTag

sealed trait ST[S, A] {
  self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B) = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a) -> s1
    }
  }

  def flatMap[B](f: A => ST[S, B]) = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }

}

object ST {
  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S): (A, S) = memo -> s
    }
  }

  implicit def runToST[S, A](f: S => (A, S)) = new ST[S, A] {
    override protected def run(s: S): (A, S) = f(s)
  }

  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}

sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: A): ST[S, Unit] = (s: S) => {
    cell = a
    () -> s
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell: A = a
  })
}


sealed abstract class STArray[S, A: ClassTag] {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.length)

  def write(i: Int, a: A): ST[S, Unit] = (s: S) => {
    value(i) = a
    ((), s)
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] = xs.foldLeft(ST[S, Unit]()) {
    case (s, (i, a)) =>
      s.flatMap(_ => write(i, a))
  }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()


}

object STArray {

  def apply[S, A: ClassTag](sz: Int, v: A): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
    lazy val value: Array[A] = Array.fill(sz)(v)
  })

  def fromList[S, A: ClassTag](xs: List[A]): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
    lazy val value = xs.toArray
  })

  def partition[S](arr: STArray[S, Int],
                   n: Int, r: Int, pivot: Int): ST[S, Int] = {
    var j = n
    for {
      piv <- arr.read(pivot)
      _ = println(piv)
      _ <- arr.swap(pivot, r)
      _ = for (i <- n.until(r)) {
        for {
          vi <- arr.read(i)
          _ <- {
            if (piv < vi) {
              j += 1
              arr.swap(i, j)
            } else ST[S, Int](0)
          }
        } yield ()
      }
    } yield j
  }


}