package chapter_14

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}
