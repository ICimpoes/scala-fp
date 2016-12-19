package chapter_7

import java.util.concurrent._

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](p: Par[A])(implicit e: ExecutorService): Future[A] = p(e)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map2WithTimeout[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C)(timeout: Long, units: TimeUnit): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get(timeout: Long, units: TimeUnit), bf.get(timeout: Long, units: TimeUnit)))
    }

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))


  private case class UnitFuture[A](get: A) extends Future[A] {

    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

}
