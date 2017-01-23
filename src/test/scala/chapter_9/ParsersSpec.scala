package chapter_9

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


class ParsersSpec extends Properties("Parsers") {

  val p = SimpleParser.parser

  import p._

  val digits = (1 to 9).map(_.toString.head)
  val myChars = ('A' to 'Z') ++ ('a' to 'z') ++ digits

  val stringGen: Gen[String] = Gen.choose(1, 100).flatMap(len => Gen.buildableOfN[String, Char](len, Gen.oneOf(myChars)))

  val nonEmpty: Gen[String] = arbitrary[String].suchThat(_.nonEmpty)

  property("char") = forAll { (c: Char) =>
    run(c)(c.toString) == Right(c)
  }

  property("string()") = forAll { (s: String) =>
      p.run(s)(s) == Right(s)
  }

  property("listOfN") = forAll(nonEmpty, nonEmpty) { (s1: String, s2: String) =>
    run(listOfN(3, s1 | s2))(s1 * 3) == Right(List.fill(3)(s1)) &&
      run(listOfN(3, s1 | s2))(s1 * 2 + s2) == Right(s1 :: s1 :: s2 :: Nil) &&
      run(listOfN(3, s1 | s2))(s1 + s2 * 2) == Right(s1 :: s2 :: s2 :: Nil) &&
      run(listOfN(3, s1 | s2))(s2 * 3) == Right(List.fill(3)(s2)) &&
      run(listOfN(3, s1 | s2))(s2 * 2 + s1) == Right(s2 :: s2 :: s1 :: Nil) &&
      run(listOfN(3, s1 | s2))(s2 + s1 * 2) == Right(s2 :: s1 :: s1 :: Nil)
  }

  property("or") = forAll(nonEmpty, nonEmpty) { (s1: String, s2: String) =>
    p.run(s1 | s2)(s1) == Right(s1) &&
    p.run(s1 | s2)(s2) == Right(s2)
  }

  property("numA") = forAll(stringGen) { (s: String) =>
    p.run(impl.numA)(s) == Right(s.takeWhile(_ == 'a').length)
  }


  property("string") = forAll(stringGen) { (s: String) =>
    p.run(string)(s""""$s"""") == Right(s)
  }

  property("succeed") = forAll { (i: Int) =>
    p.run(succeed(i))("") == Right(i)
  }

}
