package chapter_13

import chapter_13.Free._
import chapter_7.Par
import chapter_7.Par.Par

import scala.io.StdIn.readLine
import scala.util.Try

sealed trait Console[A] {
  def toPar: Par[A]

  def toThunk: () => A

  def toReader: ConsoleReader[A]
}


object Console {

  case object ReadLine extends Console[Option[String]] {
    def toPar = Par.lazyUnit(run)

    def toThunk = () => run

    def run: Option[String] = Try(readLine()).toOption

    def toReader: ConsoleReader[Option[String]] = ConsoleReader(Some(_))
  }

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar = Par.lazyUnit(println(line))

    def toThunk = () => println(line)

    def toReader: ConsoleReader[Unit] = ConsoleReader(_ => ())
  }

  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] =
    Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] =
    Suspend(PrintLine(line))

  val f1: Free[Console, Option[String]] = for {
    _ <- printLn("I can only interact with the console.")
    ln <- readLn
    _ <- printLn(s"Got $ln")
  } yield ln

  val consoleToFunction0 =
    new (Console ~> Function0) {
      def apply[A](a: Console[A]) = a.toThunk
    }

  val consoleToPar =
    new (Console ~> Par) {
      def apply[A](a: Console[A]) = a.toPar
    }

  def runConsole[A](a: ConsoleIO[A]): A =
    Free.runTrampoline(translate(a)(consoleToFunction0))

  val consoleToReader = new (Console ~> ConsoleReader) {
    def apply[A](a: Console[A]) = a.toReader
  }

  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] =
    runFree[Console, ConsoleReader, A](io)(consoleToReader)

}


object ConsoleApp extends App {

  import Console._
  import Free._

  runFree(f1)(consoleToFunction0)

}