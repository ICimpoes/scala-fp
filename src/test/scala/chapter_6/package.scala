package object chapter_6 {

  case class TestRNG(from: Int) extends RNG {
    def nextInt: (Int, RNG) = from -> TestRNG(from + 1)
  }

}
