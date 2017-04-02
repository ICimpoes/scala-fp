import chapter_15._

val p = Process.liftOne((x: Int) => x * 2)
val p2 = Process.lift((x: Int) => x * 2)
val xs = p(Stream(1, 2, 3)).toList

p2(Stream(1, 2, 3)).toList

Process.filter[Int](_ % 2 == 0)(Stream(1, 2, 3)).toList
Process.filter[Int](_ % 2 != 0)(Stream(1, 2, 3)).toList