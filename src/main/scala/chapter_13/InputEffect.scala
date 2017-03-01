package chapter_13

import chapter_13.IO._

object InputEffect extends App {

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  def converter: Unit = {
    println("Enter a temperature in degrees Fahrenheit: ")
    val d = io.StdIn.readLine().toDouble
    println(fahrenheitToCelsius(d))
  }

  def converter_ = {
    for {
      _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
      i <- readInt
      _ <- echo
      f = fahrenheitToCelsius(i)
      _ <- PrintLine(f.toString)
    } yield ()
  }

  converter_.run
//  converter

}
