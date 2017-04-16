package chapter_15.extensible

import chapter_11.Monad
import chapter_11.Monad.Ops._
import chapter_13.IO
import chapter_15.extensible.Process._

trait Process[F[_], O] {

  def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
    def go(cur: Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] =
      cur match {
        case Emit(h, t) => go(t, acc :+ h)
        case Halt(End) => F.unit(acc)
        case Halt(err) => F.fail(err)
        case Await(req, recv) => F.attempt(req).flatMap(x => go(Try(recv(x)), acc))
      }
    go(this, IndexedSeq())
  }

  def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
    case Halt(e) => Try(f(e))
    case Emit(h, t) => Emit(h, t.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
  }

  def ++(p: => Process[F, O]): Process[F, O] =
    this.onHalt {
      case End => p
      case err => Halt(err)
    }

  def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] =
    this match {
      case Halt(err) => Halt(err)
      case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
      case Await(req, recv) =>
        Await(req, recv andThen (_ flatMap f))
    }

  def onComplete(p: => Process[F, O]): Process[F, O] =
    this.onHalt {
      case End => p.asFinalizer
      case err => p.asFinalizer ++ Halt(err)
    }

  def asFinalizer: Process[F, O] = this match {
    case Emit(h, t) => Emit(h, t.asFinalizer)
    case Halt(e) => Halt(e)
    case Await(req, recv) => await(req) {
      case Left(Kill) => this.asFinalizer
      case x => recv(x)
    }
  }

  def drain[O2]: Process[F, O2] = this match {
    case Halt(err) => Halt(err)
    case Emit(h, t) => t.drain
    case Await(r, f) => await(r) { x => f(x).drain }
  }
}

object Process {

  trait MonadCatch[F[_]] extends Monad[F] {
    def attempt[A](a: F[A]): F[Either[Throwable, A]]

    def fail[A](t: Throwable): F[A]
  }

  def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
    try p
    catch {
      case e: Throwable => Halt(e)
    }

  def await[F[_], A, O](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] =
    Await(req, recv)

  case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => Process[F, O])
    extends Process[F, O]

  case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]

  case class Halt[F[_], O](err: Throwable) extends Process[F, O]

  case object End extends Exception

  case object Kill extends Exception


  def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] = IO {
    @annotation.tailrec
    def go(cur: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] =
      cur match {
        case Emit(h, t) => go(t, acc :+ h)
        case Halt(End) => acc
        case Halt(err) => throw err
        case Await(req, recv) =>
          val next =
            try recv(Right(req))
            catch {
              case err: Throwable => recv(Left(err))
            }
          go(next, acc)
      }
    try go(src, IndexedSeq())
    finally ()
  }

  import java.io.{BufferedReader, FileReader}

  def p(fileName: String): Process[IO, String] =
    await(IO(new BufferedReader(new FileReader(fileName)))) {
      case Right(b) =>
        lazy val next: Process[IO, String] = await(IO(b.readLine)) {
          case Left(e) => await(IO(b.close))(_ => Halt(e))
          case Right(line) =>
            if (line eq null) Halt(End)
            else Emit(line, next)
        }
        next
      case Left(e) => Halt(e)
    }

  def resource[R, O](acquire: IO[R])(use: R => Process[IO, O])(release: R => Process[IO, O]): Process[IO, O] =
    eval(acquire).flatMap(r => use(r).onComplete(release(r)))

  def eval[F[_], A](a: F[A]): Process[F, A] = await(a) {
    case Right(o) => Emit(o, Halt(End))
    case Left(err) => Halt(err)
  }

  def eval_[F[_], A, B](a: F[A]): Process[F, B] = eval(a).drain
}
