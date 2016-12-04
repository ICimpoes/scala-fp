package chapter_2

object TestCompose extends App {

  println(compose((x: Int) => (x + 2).toDouble, (s: String) => s.toInt + 2)("2"))



}
