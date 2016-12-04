package chapter_3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, tail) => tail
      case _ => sys.error("Empty list does not have a tail")
    }
  }

  def setHead[A](head: A, l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("Cannot set head on empty List")
      case Cons(_, t) => Cons(head, t)
    }
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(_, t) =>
          drop(t, n - 1)
      }
    }
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("cannot call init on empty list")
      case Cons(last, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }


  def foldRightLazy[A, B](as: List[A], z: B)(f: (A, => B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRightLazy(xs, z)(f))
    }

  def p(a: Double, b: => Double) = {
    println(s"a = $a")
    if (a == 0.0) 0.0
    else a * b
  }


  def product2(ns: List[Double]) =
    foldRightLazy(ns, 1.0)(p)

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, b) => b + 1)


  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {

    @tailrec
    def loop(s: List[A], acc: B): B = {
      s match {
        case Nil => acc
        case Cons(h, t) =>
          loop(t, f(acc, h))
      }
    }

    loop(as, z)
  }

  def sum2(l: List[Double]): Double =
    foldLeft(l, 0.0)(_ + _)

  def product3(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def foldRightUsingFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeftUsingFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def appendUsingFR[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def appendUsingFL[A](l: List[A], r: List[A]): List[A] =
    foldLeft(reverse(l), r)((t, h) => Cons(h, t))

  def concat[A](lists: List[List[A]]) =
    foldRight(lists, Nil: List[A])(appendUsingFR)

  def add_1(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((h, l) => Cons(h + 1, l))

  def doubleToString(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((h, l) => Cons(h.toString, l))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, l) => Cons(f(h), l))

  def map_1[A,B](as: List[A])(f: A => B): List[B] =
    foldRightUsingFoldLeft(as, Nil: List[B])((h, l) => Cons(f(h), l))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightUsingFoldLeft(as, Nil: List[A])((h, l) => if (f(h)) Cons(h, l) else l)


  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRightUsingFoldLeft(as, Nil: List[B])((h, l) => appendUsingFL(f(h), l))

  def flatMap_1[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map_1(as)(f))

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as){ x => if (f(x)) List(x) else Nil }

  def zip[A, B](f: List[A], s: List[B]) = {
    @tailrec
    def go(l1: List[A], l2: List[B], res: List[(A, B)]): List[(A, B)] = {
      l1 -> l2 match {
        case (Cons(h1, t1), Cons(h2, t2)) =>
          go(t1, t2, appendUsingFL(res, List(h1 -> h2)))
        case _ =>
          res

      }
    }
    go(f, s, Nil)
  }


  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    @tailrec
    def go(l1: List[A], l2: List[B], res: List[C]): List[C] = {
      l1 -> l2 match {
        case (Cons(h1, t1), Cons(h2, t2)) =>
          go(t1, t2, appendUsingFL(res, List(f(h1, h2))))
        case _ =>
          res

      }
    }
    go(a, b, Nil)
  }


  @tailrec
  def startsWith[A](a: List[A], b: List[A]): Boolean = a -> b match {
    case (Cons(h1, t1), Cons(h2, t2)) =>
      if (h1 == h2) startsWith(t1, t2)
      else false
    case (_, Nil) => true
    case _ => false
  }

  @tailrec
  def hasSubSequence[A](a: List[A], b: List[A]): Boolean = a match {
    case Nil => b == Nil
    case Cons(h, t) =>
      if (startsWith(a, b)) true
      else hasSubSequence(t, b)
    case _ => false
  }


}
