package chapter_13

import chapter_13.Free._

trait Files[A]

case class OpenRead(file: String) extends Files[HandleR]

case class OpenWrite(file: String) extends Files[HandleW]

case class ReadLine(h: HandleR) extends Files[Option[String]]

case class WriteLine(h: HandleW, line: String) extends Files[Unit]

trait HandleR

trait HandleW

object ConvertApp extends App {

  val fahrenheitToCelsius: Double => Double = ???

  def loop(f: HandleR, c: HandleW): Free[Files, Unit] = for {
    line <- Suspend(ReadLine(f))
    _ <- line match {
      case None => Return[Files, Unit](())
      case Some(s) => Suspend {
        WriteLine(c, fahrenheitToCelsius(s.toDouble).toString)
      } flatMap (_ => loop(f, c))
    }
  } yield ()

  def convertFiles = for {
    f <- Suspend(OpenRead("fahrenheit.txt"))
    c <- Suspend(OpenWrite("celsius.txt"))
    _ <- loop(f,c)
  } yield ()

}