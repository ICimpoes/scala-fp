package chapter_10

sealed trait WC {
  def toInt: Int
}

case class Stub(chars: String) extends WC {
  override def toInt: Int = unsub(chars)
}

case class Part(lStub: String, words: Int, rStub: String) extends WC {
  override def toInt: Int = unsub(lStub) + unsub(rStub) + words
}