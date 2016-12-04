import scala.annotation.tailrec

package object chapter_2 {

  def fib(n: Int): Int = {

    @tailrec
    def go(num: Int, current: Int, next: Int): Int = {

      if (num <= 0) current
      else go(num - 1, next, current + next)

    }

    go(n, 0, 1)

  }


  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(i: Int): Boolean = {
      if (i + 1 >= as.length) true
      else if (ordered(as(i), as(i + 1))) loop(i + 1)
      else false
    }
    loop(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }


}
