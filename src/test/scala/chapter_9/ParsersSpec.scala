package chapter_9

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary._


class ParsersSpec extends Properties("Parsers") {

  val p = SimpleParser.parser

  import p._

  val nonEmpty: Gen[String] = arbitrary[String].suchThat(_.nonEmpty)
  val byReg: Gen[String] = arbitrary[String].suchThat(s => "[a-zA-Z0-9_ ']+".r.findFirstMatchIn(s).nonEmpty)

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

//  property("numA") = forAll(byReg) { (s: String) =>
//    p.run(impl.numA)(s) == Right(s.count(_ == 'a'))
//  }
//
//  property("string") = forAll(byReg) { (s: String) =>
//    p.run(string)(s""""$s"""") == Right(s)
//  }

  property("succeed") = forAll { (i: Int) =>
    p.run(succeed(i))("") == Right(i)
  }

}
