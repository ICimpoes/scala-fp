package chapter_13

import chapter_13.IO._

object Factorial extends App {

  def factorial(n: Int): IO[Int] = {
    for {
      acc <- ref(1)
      _ <- foreachM(1 to n toStream)(i => skip(acc.modify(_ * i)))
      result <- acc.get
    } yield result
  }

  val factorialREPL: IO[Unit] = sequence_(
    PrintLine("Factorial"),
    doWhile {
      ReadLine
    } { line =>
      when[Unit, IO](line != "q") {
        val in = line.toInt
        for {
          n <- factorial(in)
          _ <- PrintLine(s"Factorial of $in is: $n")
        } yield ()
      }
    })


  factorialREPL.run
}
