import chapter_15._

val stream: Stream[Int] = Stream(1, 2, 3)

val p = Process.liftOne((x: Int) => x * 2)
val p2 = Process.lift((x: Int) => x * 2)
val xs = p(stream).toList

p2(stream).toList

Process.filter[Int](_ % 2 == 0)(stream).toList
Process.filter[Int](_ % 2 != 0)(stream).toList

Process.sum(stream.map(_.toDouble)).toList

Process.take(3)(stream).toList
Process.take(2)(stream).toList