import chapter_9.JSON._

package object chapter_9 {

  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._

    def jNull = string("null").map(_ => JNull).<*>

    def jNumber: Parser[JNumber] = double.map(JNumber(_)).<*>

    def jString: Parser[JString] = string.map(JString(_)).<*>

    def jBool: Parser[JBool] = (string("true") | string("false")).map(s => JBool(s.toBoolean)).<*>

    def jArray: Parser[JArray] = '['
      .<*>
      .skipThis(
        (jNull |
          jNumber |
          jString |
          jBool |
          jsObj |
          jArray)
          .manyWithSeparator(",")
      ).skipThat(']'.<*>)
      .map(l => JArray(l.toIndexedSeq))

    def jsObj: Parser[JObject] = '{'.<*>.skipThis {
      string.skipThat(':'.<*>).**(jNull | jNumber | jString | jBool | jsObj | jArray).manyWithSeparator(",").map(l => JObject(l.toMap))
    }.skipThat('}').<*>

    jsObj | jArray

  }

}
