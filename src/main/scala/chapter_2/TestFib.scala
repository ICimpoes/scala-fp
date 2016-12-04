package chapter_2

object TestFib extends App {

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  println(formatResult("fib", 4, fib))

}
