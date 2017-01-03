package chapter_7

import java.util.concurrent.Executors

import chapter_7.Par._
import chapter_8._
import chapter_8.Gen.choose
import org.scalatest.{FlatSpec, Matchers}

class ParSpec extends FlatSpec with Matchers {

  implicit def toUnitPar[A](a: A): Par[A] = unit(a)

  implicit val es = Executors.newCachedThreadPool

  "Par.unit" should "create a Par[A] from A" in {
    val res = Gen.unit(Par.unit(1)).forAll(i =>
      i.map(_ + 1).get == Par.unit(2).get)

    res.exec() shouldBe Passed
    Prop.check(
      Par.unit(1).map(_ + 1).equal(Par.unit(2)).get
    ).exec() shouldBe Proved

    Gen.checkPar(Par.unit(1).map(_ + 1).equal(Par.unit(2))).exec() shouldBe Passed
  }

  "Par.map" should "map over Par" in {
    val pint = Gen.choose(0, 10) map Par.unit
    val p = pint.forAllPar(n => Par.map(n)(y => y).equal(n))

    p.exec() shouldBe Passed

  }

  "Par.fork" should "fork a A to Par[A]" in {
    val pint: Gen[Par[Int]] = Gen.choose(0, 10) map unit
    val p = pint.forAllPar(n => n.equal(fork(n)))

    p.exec() shouldBe Passed
  }

  "Par.parMap" should "map over elements of list in parallel" in {
    val pint: Gen[List[Int]] = choose(-10, 10).listOfN(choose(0, 4))
    val p = pint.forAllPar { l =>
      parMap(l)(_ * 2).equal(unit(l.map(_ * 2)))
    }

    p.exec(2, 2) shouldBe Passed
  }

  "Par.sequence" should "transform List[Par] int Par[List]" in {
    val pint: Gen[List[Int]] = choose(-10, 10).listOfN(choose(0, 4))
    val p = pint.forAllPar { l =>
      sequence(l.map(unit)).equal(unit(l))
    }

    p.exec(2, 2) shouldBe Passed
  }

  "Par.parFilter" should "filter elements of list in parallel" in {
    val pint: Gen[List[Int]] = choose(-10, 10).listOfN(choose(0, 4))
    val oddFilt = (_: Int) % 2 == 0
    val p = pint.forAllPar { l =>
      parFilter(l)(oddFilt).equal(unit(l.filter(oddFilt)))
    }

    p.exec(2, 2) shouldBe Passed
  }

  "Par.choice" should "choice a Par based on boolean" in {
    val intIntBool: Gen[((Int, Int), Boolean)] = choose(-10, 10).**(choose(-10, 10)).**(Gen.boolean)

    val p = intIntBool forAllPar { case ((x, y), b) =>
      choice(b)(x, y).equal(if (b) x else y)
    }

    p.exec(2, 2) shouldBe Passed
  }

  "Par.choiceUsingCN" should "choice a Par based on boolean" in {
    val intIntBool: Gen[((Int, Int), Boolean)] = choose(-10, 10).**(choose(-10, 10)).**(Gen.boolean)

    val p = intIntBool forAllPar { case ((x, y), b) =>
      choiceUsingCN(b)(x, y).equal({
        if (b) x else y
      })
    }

    p.exec(2, 2) shouldBe Passed
  }

  "Par.choiceUsingChooser" should "choice a Par based on boolean" in {
    val intIntBool: Gen[((Int, Int), Boolean)] = choose(-10, 10).**(choose(-10, 10)).**(Gen.boolean)

    val p = intIntBool forAllPar { case ((x, y), b) =>
      choiceUsingChooser(b)(x, y).equal({
        if (b) x else y
      })
    }

    p.exec(2, 2) shouldBe Passed
  }

  "Par.choiceN" should "choice n-th element of a list" in {
    val intList: Gen[(Int, List[Int])] = choose(0, 3).**(choose(-10, 10).listOfN(choose(3, 5)))

    val p = intList forAllPar { case (x, l) =>
      choiceN(x)(l.map(x => x: Par[Int])).equal(l(x))
    }

    p.exec(2, 2) shouldBe Passed
  }

  "Par.choiceNUsingChooser" should "choice n-th element of a list" in {
    val intList: Gen[(Int, List[Int])] = choose(0, 3).**(choose(-10, 10).listOfN(choose(3, 5)))

    val p = intList forAllPar { case (x, l) =>
      choiceNUsingChooser(x)(l.map(x => x: Par[Int])).equal(l(x))
    }

    p.exec(2, 2) shouldBe Passed
  }

  "Par.chooser" should "map and flatten" in {
    val intList: Gen[(Int, List[Int])] = choose(0, 3).**(choose(-10, 10).listOfN(choose(3, 5)))

    val p = intList forAllPar { case (x, l) =>
      chooser(x)(a => unit(l.map(_ + a))).equal(l.map(_ + x))
    }

    p.exec(2, 2) shouldBe Passed
  }

  "Par.flatMap" should "map and flatten" in {
    val intList: Gen[(Int, List[Int])] = choose(0, 3).**(choose(-10, 10).listOfN(choose(3, 5)))

    val p = intList forAllPar { case (x, l) =>
      flatMap(x)(a => unit(l.map(_ + a))).equal(l.map(_ + x))
    }

    p.exec(2, 2) shouldBe Passed
  }

  "Par.flatMapUsingJoin" should "map and flatten" in {
    val intList: Gen[(Int, List[Int])] = choose(0, 3).**(choose(-10, 10).listOfN(choose(0, 5)))

    val p = intList forAllPar { case (x, l) =>
      flatMapUsingJoin(x)(a => unit(l.map(_ + a))).equal(l.map(_ + x))
    }

    p.exec(2, 2) shouldBe Passed
  }

  "Par.join" should "transform Par[Par[A]] into Par[A] (flatten)" in {
    val intList: Gen[(Int, List[Int])] = choose(0, 3).**(choose(-10, 10).listOfN(choose(0, 5)))

    val p = intList forAllPar { case (x, l) =>
      join(unit(l).map[Par[List[Int]]](pl => unit(pl.map(_ + x)))).equal(l.map(_ + x))
    }

    p.exec(2, 2) shouldBe Passed
  }

  "Par.joinUsingFlatMap" should "transform Par[Par[A]] into Par[A] (flatten)" in {
    val intList: Gen[(Int, List[Int])] = choose(0, 3).**(choose(-10, 10).listOfN(choose(0, 5)))

    val p = intList forAllPar { case (x, l) =>
      joinUsingFlatMap(unit(l).map[Par[List[Int]]](pl => unit(pl.map(_ + x)))).equal(l.map(_ + x))
    }

    p.exec(2, 2) shouldBe Passed
  }
}
