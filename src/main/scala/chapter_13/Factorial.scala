package chapter_13

import chapter_13.IO._
import chapter_11.Monad._

object Factorial extends App {
//
//  def factorial(n: Int): IO[Int] = for {
//    acc <- ref(1)
//    _ <- foreachM (1 to n toStream) (i => acc.modify(_ * i).skip)
//    result <- acc.get
//  } yield result
//
//  val factorialREPL: IO[Unit] = sequence_(
//    PrintLine("Factorial"),
//    doWhile { ReadLine } { line =>
//      when (line != "q") { for {
//        n <- factorial(line.toInt)
//        _ <- IO { println("factorial: " + n) }
//      } yield () }
//    })

}
