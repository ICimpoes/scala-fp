package chapter_9

case class Location(input: String, offset: Int = 0) {

  def slice(length: Int) = input.slice(offset, offset + length)

  val left = input.drop(offset)

  def move(length: Int): Location = this.copy(offset = offset + length)

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1

  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

}
