package chapter_7

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}

import scala.util.Try

object NonBlocking {

  type Future[A] = (A => Unit) => Unit

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a =>
      Try(ref.set(a))
      latch.countDown()
    }
    latch.await()
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es => (cb: A => Unit) => cb(a)

  def fork[A](a: => Par[A]): Par[A] =
    es => (cb: A => Unit) => eval(es)(a(es)(cb))

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call = r
    })


  def map2WithActor[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es => (cb: C => Unit) => {
      var ar: Option[A] = None
      var br: Option[B] = None
      val combiner = Actor[Either[A, B]](es) {
        case Left(a) => br match {
          case None => ar = Some(a)
          case Some(b) => eval(es)(cb(f(a, b)))
        }
        case Right(b) => ar match {
          case None => br = Some(b)
          case Some(a) => eval(es)(cb(f(a, b)))
        }
      }
      p(es)(a => combiner ! Left(a))
      p2(es)(b => combiner ! Right(b))
    }
}


object A extends App {

  import NonBlocking._

  val es: ExecutorService = Executors.newSingleThreadExecutor()

  val p1 = fork(unit {
    Thread.sleep(1000)
    println("5")
    5
  })

  val p2 = fork(unit {
    Thread.sleep(1000)
    throw new Exception("eas")
    println("6")
    6
  })

  println("Wait and do nothing")

  Thread.sleep(1000)

  println("Now start!")

  println("res: " + run(es)(map2WithActor(p1, p2)(_ + _)))

  println("finished!")

}