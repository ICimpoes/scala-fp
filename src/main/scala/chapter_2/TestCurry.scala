package chapter_2

object TestCurry extends App {



  val curred = curry((_: Int).toString + (_: Int).toString)

  println(curred(1)(2))

  println(uncurry(curred)(1, 2))


}
