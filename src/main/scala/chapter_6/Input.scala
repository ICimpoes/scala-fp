package chapter_6

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  import chapter_6.State._

  def update(in: Input)(machine: Machine): Machine =
    machine -> in match {
      case (Machine(true, candies, coins), Coin) if candies > 0 =>
        Machine(false, candies, coins + 1)
      case (Machine(false, candies, coins), Turn) if candies > 0 =>
        Machine(true, candies - 1, coins)
      case (m, _) =>
        m
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(in => modify[Machine](update(in))))
    machine <- get
  } yield machine.coins -> machine.candies

}
