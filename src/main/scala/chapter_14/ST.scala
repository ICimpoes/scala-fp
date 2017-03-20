package chapter_14

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B) = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a) -> s1
    }
  }

  def flatMap[B](f: A => ST[S, B]) = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }

}

object ST {
  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S): (A, S) = memo -> s
    }
  }

  implicit def runToST[S, A](f: S => (A, S)) = new ST[S, A] {
    override protected def run(s: S): (A, S) = f(s)
  }
}

sealed trait STRef[S, A] {
  protected var cell: A

  def Read: ST[S, A] = ST(cell)

  def Write(a: A): ST[S, Unit] = (s: S) => {
    cell = a
    () -> s
  }
}

