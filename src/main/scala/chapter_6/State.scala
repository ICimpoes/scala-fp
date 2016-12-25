package chapter_6

case class State[S, +A](run: S => (A, S)) {

  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.flatMap(b => unit(f(a, b))))

}

object State {

  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)

  def nonNegativeLessThan(n: Int) = State(RNG.nonNegativeLessThan(n))

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def unit[S, A](a: A): State[S, A] =
    State(a -> _)

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))((a, acc) => a.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  val ns: Rand[List[Int]] =
    int.flatMap(x =>
      int.flatMap(y =>
        ints(x).map(xs =>
          xs.map(_ % y))))

  val ns2: Rand[List[Int]] = for {
    x <- int
    y <- int
    xs <- ints(x)
  } yield xs.map(_ % y)


}
