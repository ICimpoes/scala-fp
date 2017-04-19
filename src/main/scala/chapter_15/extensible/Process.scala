package chapter_15.extensible

import java.io.FileWriter

import chapter_11.Monad
import chapter_11.Monad.Ops._
import chapter_13.IO
import chapter_15.extensible.Process._
import chapter_15.extensible.Process1.Process1
import chapter_15.extensible.T.Tee

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

  final def drain[O2]: Process[F, O2] = this match {
    case Halt(err) => Halt(err)
    case Emit(h, t) => t.drain
    case Await(r, f) => await(r) { x => f(x).drain }
  }

  def repeat: Process[F, O] = this ++ this.repeat

  def |>[O2](p2: Process1[O, O2]): Process[F, O2] = p2 match {
    case Halt(e) => this.kill onHalt (e2 => Halt(e) ++ Halt(e2))
    case Emit(h, t) => Emit(h, this |> t)
    case Await(req, recv) => this match {
      case Halt(err) => Halt(err) |> recv(Left(err))
      case Emit(h, t) => t |> Try(recv(Right(h)))
      case Await(req0, recv0) => await(req0)(recv0 andThen (_ |> p2))
    }
  }

  def pipe[O2](p2: Process1[O, O2]): Process[F, O2] =
    this |> p2

  def map[O2](f: O => O2): Process[F, O2] =
    this |> Process1.lift(f)

  @annotation.tailrec
  final def kill[O2]: Process[F, O2] = this match {
    case Await(req, recv) => recv(Left(Kill)).drain.onHalt {
      case Kill => Halt(End)
      case e => Halt(e)
    }
    case Halt(e) => Halt(e)
    case Emit(h, t) => t.kill
  }

  def filter(f: O => Boolean): Process[F, O] =
    this |> Process1.filter(f)

  def tee[O2, O3](p2: Process[F, O2])(t: Tee[O, O2, O3]): Process[F, O3] =
    t match {
      case Halt(e) => this.kill onComplete p2.kill onComplete Halt(e)
      case Emit(h, t) => Emit(h, (this tee p2) (t))
      case Await(side, recv) => side.get match {
        case Left(isO) => this match {
          case Halt(e) => p2.kill onComplete Halt(e)
          case Emit(o, ot) => (ot tee p2) (Try(recv(Right(o))))
          case Await(reqL, recvL) =>
            await(reqL)(recvL andThen (this2 => this2.tee(p2)(t)))
        }
        case Right(isO2) => p2 match {
          case Halt(e) => this.kill onComplete Halt(e)
          case Emit(o2, ot) => this.tee(ot)(Try(recv(Right(o2))))
          case Await(reqR, recvR) =>
            await(reqR)(recvR andThen (p3 => this.tee(p3)(t)))
        }
      }
    }

  def zipWith[O2, O3](p2: Process[F, O2])(f: (O, O2) => O3): Process[F, O3] =
    (this tee p2) (T.zipWith(f))

  def zip[O2](p2: Process[F, O2]): Process[F, (O, O2)] =
    zipWith(p2)((_, _))

  def to[O2](sink: Sink[F, O]): Process[F, Unit] =
    join((this zipWith sink) ((o, f) => f(o)))

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

  def emit[F[_], O](head: O, tail: Process[F, O] = Halt[F, O](End)): Process[F, O] =
    Emit(head, tail)

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

  def lines(filename: String): Process[IO, String] = resource(IO(io.Source.fromFile(filename))) {
    src =>
      lazy val iter = src.getLines // a stateful iterator
    def step = if (iter.hasNext) Some(iter.next) else None
      lazy val lines: Process[IO, String] = eval(IO(step)).flatMap {
        case None => Halt(End)
        case Some(line) => Emit(line, lines)
      }
      lines
  } { src => eval_(IO(src.close)) }

  type Sink[F[_], O] = Process[F, O => Process[F, Unit]]

  def fileW(file: String, append: Boolean = false): Sink[IO, String] =
    resource[FileWriter, String => Process[IO, Unit]](IO(new FileWriter(file, append))) { w =>
      constant(s => eval[IO, Unit](IO(w.write(s))))
    }(w => eval_(IO(w.close)))

  def constant[A](a: A): Process[IO, A] =
    eval[IO, A](IO(a)).repeat

  def join[F[_], O](p: Process[F, Process[F, O]]): Process[F, O] = p.flatMap(identity)

  import Process1._

  def intersperse[I](sep: I): Process1[I, I] =
    await1[I, I](i => emit1(i) ++ id.flatMap(i => emit1(sep) ++ emit1(i)))

  val converter: Process[IO, Unit] =
    lines("fahrenheit.txt")
      .filter(!_.startsWith("#"))
      .map(line => chapter_15.Process.toCelsius(line.toDouble).toString)
      .pipe(intersperse("\n"))
      .to(fileW("celsius.txt"))
      .drain

}
