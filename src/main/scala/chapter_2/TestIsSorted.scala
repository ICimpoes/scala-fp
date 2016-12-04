package chapter_2

object TestIsSorted extends App {

  println(isSorted[Int](Array(1, 2, 3), _ < _))
  println(isSorted[Int](Array(2, 3, 1), _ < _))
  println(isSorted[Int](Array(3, 2, 1), _ > _))
  println(isSorted[Int](Array(1), _ < _))

}
