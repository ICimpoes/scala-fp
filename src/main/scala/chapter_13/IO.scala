package chapter_13

trait IO { self =>

  def run: Unit

  def ++(other: IO) = new IO {
    def run: Unit = {
      self.run
      other.run
    }
  }

}

object IO {

  def PrintLine(line: String) = new IO {
    def run: Unit = println(line)
  }

  def empty: IO = new IO {
    def run: Unit = ()
  }

}
