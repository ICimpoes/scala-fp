package chapter_14

import chapter_14.Util._

import scala.collection.mutable
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


object Util {

  case class Debug(debug: Boolean)

  def noop[S] = ST[S, Unit](())

  def debug(any: Any)(implicit d: Debug) = if (d.debug) println(any)

}

object STArray {


  def apply[S, A: ClassTag](sz: Int, v: A): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
    lazy val value: Array[A] = Array.fill(sz)(v)
  })

  def fromList[S, A: ClassTag](xs: List[A]): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
    lazy val value = xs.toArray
  })

  def partition[S](arr: STArray[S, Int],
                   n: Int, r: Int, pivot: Int)(implicit d: Debug): ST[S, Int] = {
    for {
      piv <- arr.read(pivot)
      _ <- arr.swap(pivot, r)
      j <- STRef[S, Int](n)
      _ <- (n until r).foldLeft(noop[S])((s, i) =>
        for {
          _ <- s
          c <- arr.read(i)
          _ <- {
            if (c < piv) for {
              b <- j.read
              _ <- arr.swap(i, b)
              _ <- j.write(b + 1)
            } yield ()
            else noop[S]
          }
        } yield ())
      a <- j.read
      _ <- arr.swap(r, a)
    } yield a
  }

  def qs[S](a: STArray[S, Int], l: Int, r: Int)(implicit d: Debug): ST[S, Unit] = {
    if (r <= l) noop[S]
    else for {
      piv <- partition(a, l, r, (l + r) / 2)
      _ = debug(s"piv = $piv")
      _ <- qs(a, l, piv - 1)
      _ <- qs(a, piv + 1, r)
    } yield ()
  }

  def quicksort(xs: List[Int])(implicit d: Debug): List[Int] =
    if (xs.isEmpty) xs
    else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr <- STArray.fromList(xs)
        size <- arr.size
        _ <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })

}

sealed abstract class STMap[S, K, V] {

  protected def value: mutable.Map[K, V]

  def size: ST[S, Int] = ST(value.size)

  def apply(k: K): ST[S, V] = ST(value(k))

  def get(k: K): ST[S, Option[V]] = ST(value.get(k))

  def +=(k: K, v: V): ST[S, Unit] = ST(value.update(k, v))

  def -=(k: K): ST[S, Unit] = ST(value -= k)

  def contains(k: K): ST[S, Boolean] = ST(value.contains(k))
}

object STMap {
  def empty[S, K, V]: ST[S, STMap[S, K, V]] = ST(new STMap[S, K, V] {
    val value = mutable.HashMap.empty[K, V]
  })

  def fromMap[S, K, V](m: Map[K, V]): ST[S, STMap[S, K, V]] = ST(new STMap[S, K, V] {
    val value = (mutable.HashMap.newBuilder[K, V] ++= m).result
  })
}