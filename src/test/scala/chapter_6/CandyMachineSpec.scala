package chapter_6

import org.scalatest.{FlatSpec, Matchers}
import chapter_6.Machine._


class CandyMachineSpec extends FlatSpec with Matchers {

  "CandyMachine" should "collect coins and return candies" in {
    val ((coins, candies), machine) = simulateMachine(List(Coin, Turn, Coin, Turn)).run(Machine(true, 10, 0))

    coins shouldBe 2
    candies shouldBe 8

    machine shouldBe Machine(true, 8, 2)
  }

  "CandyMachine" should "not collect coins if machine is unlocked" in {
    val unlockedMachine = Machine(false, 2, 2)
    val ((coins, candies), machine) = simulateMachine(List(Coin, Coin, Coin, Coin, Coin)).run(unlockedMachine)

    coins shouldBe unlockedMachine.coins
    candies shouldBe unlockedMachine.candies

    machine shouldBe unlockedMachine
  }

  "CandyMachine" should "return candy on Turn if it's unlocked" in {
    val unlockedMachine = Machine(false, 2, 2)
    val ((coins, candies), machine) = simulateMachine(List(Turn)).run(unlockedMachine)

    coins shouldBe unlockedMachine.coins
    candies shouldBe unlockedMachine.candies - 1

    machine shouldBe Machine(true, candies, coins)
  }

  "CandyMachine" should "not collect coins if there is no candy left" in {
    val emptyMachine = Machine(true, 0, 2)
    val ((coins, candies), machine) = simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin)).run(emptyMachine)

    coins shouldBe emptyMachine.coins
    candies shouldBe emptyMachine.candies

    machine shouldBe emptyMachine
  }

  "CandyMachine" should "not return candy if machine is locked" in {
    val lockedMachine = Machine(true, 0, 2)
    val ((coins, candies), machine) = simulateMachine(List(Turn, Turn)).run(lockedMachine)

    coins shouldBe lockedMachine.coins
    candies shouldBe lockedMachine.candies

    machine shouldBe lockedMachine
  }

}
