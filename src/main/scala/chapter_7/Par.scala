package chapter_7

import java.util.concurrent._

object Par {

  type Par[A] = ExecutorService => Future[A]

  implicit def convertPar[A](p: Par[A]): ParOps[A] = new ParOps[A] {
    val inst: Par[A] = p
  }

  trait ParOps[A] {

    def inst: Par[A]

    def run(implicit e: ExecutorService): Future[A] = Par.run(inst)

    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] =
      Par.map2(inst, b)(f)

    def map3[B, C, D](b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
      Par.map3(inst, b, c)(f)

    def map4[B, C, D, E](b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
      Par.map4(inst, b, c, d)(f)

    def map5[B, C, D, E, F](b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
      Par.map5(inst, b, c, d, e)(f)

    def map[B](f: A => B): Par[B] =
      Par.map(inst)(f)

    def map2WithTimeout[B, C](b: Par[B])(f: (A, B) => C)(timeout: Long, units: TimeUnit): Par[C] =
      Par.map2WithTimeout(inst, b)(f)(timeout, units)

    def equal(e: ExecutorService)(p2: Par[A]): Boolean =
      inst(e).get == p2(e).get

  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](p: Par[A])(implicit e: ExecutorService): Future[A] = p(e)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
    a.map2(b)((aa, bb) => f.curried(aa)(bb)).map2(c)((fc, cc) => fc(cc))

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
    a.map3(b, c)((aa, bb, cc) => f.curried(aa)(bb)(cc)).map2(d)((fd, dd) => fd(dd))

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
    a.map4(b, c, d)((aa, bb, cc, dd) => f.curried(aa)(bb)(cc)(dd)).map2(e)((fe, ee) => fe(ee))

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit(()))((a, _) => f(a))

  def map2WithTimeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C)(timeout: Long, units: TimeUnit): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get(timeout: Long, units: TimeUnit), bf.get(timeout: Long, units: TimeUnit)))
    }

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = as.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]())) { (pa, pl) =>
      pa.map2(pl)((a, l) => a :: l)
    }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    sequence(as.map(asyncF(a => if (f(a)) Some(a) else None))).map(_.flatten)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  private case class UnitFuture[A](get: A) extends Future[A] {

    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

}
