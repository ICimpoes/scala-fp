package chapter_9

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll


class ParsersSpec extends Properties("Parsers") {

  val parsers = new Parsers[String, Parser] {}

  import parsers._

// Not Implemented
//  property("char") = forAll { (c: Char) =>
//    run(c)(c.toString) == Right(c)
//  }

//  property("string") = forAll { (s: String) =>
//    run(s)(s) == Right(s)
//  }

//  property("or") = forAll { (s1: String, s2: String) =>
//    run(s1 | s2)(s1) == Right(s1) &&
//    run(s1 | s2)(s2) == Right(s2)
//  }

}
