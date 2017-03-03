package chapter_13

import chapter_13.IO._
import chapter_11.Monad._

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
        for {
          n <- factorial(line.toInt)
          _ <- PrintLine(s"Factorial: $n")
        } yield ()
      }
    })


  factorialREPL.run
}
