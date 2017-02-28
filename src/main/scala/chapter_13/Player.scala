package chapter_13

case class Player(name: String, score: Int)

object Contest {

  def contest(p1: Player, p2: Player): Unit = {
    if (p1.score > p2.score)
      println(s"${p1.name} is a winner!")
    else if (p2.score > p1.score)
      println(s"${p2.name} is a winner!")
    else
      println("It's a tie")
  }

  def winner(p1: Player, p2: Player): Option[Player] = {
    if (p1.score > p2.score)
      Some(p1)
    else if (p2.score > p1.score)
      Some(p2)
    else
      None
  }

  def contest2(p1: Player, p2: Player): Unit = {
    winner(p1, p2).fold(println("It's a tie"))(w => println(s"${w.name} is a winner"))
  }

  def winnerMsg(maybeWinner: Option[Player]): String =
    maybeWinner.fold("It's a tie")(w => s"${w.name} is a winner")

  def contest3(p1: Player, p2: Player): Unit =
    println(winnerMsg(winner(p1, p2)))

  def contest4(p1: Player, p2: Player): IO =
    IO.PrintLine(winnerMsg(winner(p1, p2)))

}
