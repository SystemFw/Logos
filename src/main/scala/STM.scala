package stm

object Y {
  import cats._, implicits._
  import cats.free._
  import cats.effect._, concurrent._
  import scala.collection.mutable

  trait STM_[A]
  case class NewTVar[A](a: A) extends STM_[TVar[A]]
  case class ReadTVar[A](t: TVar[A]) extends STM_[A]
  case class WriteTVar[A](t: TVar[A], v: A) extends STM_[Unit]

  type STM[A] = Free[STM_, A]

  def newTVar[A](a: A): STM[TVar[A]] =
   Free.liftF[STM_, TVar[A]](NewTVar(a))

  def readTVar[A](t: TVar[A]): STM[A] =
    Free.liftF(ReadTVar(t))

  def writeTVar[A](t: TVar[A], a: A): STM[Unit] =
    Free.liftF[STM_, Unit](WriteTVar(t, a))


  /** Represents a unique identifier (using object equality). */
  private[stm] final class Token extends Serializable {
    override def toString: String = s"Token(${hashCode.toHexString})"
  }

  class TVar[A](
    var a: A,
    val id: Token
  )

  private[stm] final class Entry[A](val tvar: TVar[A], val initial: A, var current: A)
  private[stm] final class TRec(val entries: mutable.Map[Token, Entry[Any]]) {
    def insert[A](tvar: TVar[A], v: A): Unit = {
      entries += tvar.id -> new Entry(tvar.asInstanceOf[TVar[Any]], v, v)
      ()
    }

  }

  def interpreter: IO[STM_ ~> IO] =
    IO(new TRec(mutable.Map.empty)).map { trec =>
      new (STM_ ~> IO) {
        def apply[A](prog: STM_[A]): IO[A] = prog match {
          case NewTVar(a) => IO(new TVar(a, new Token))
          case ReadTVar(tvar) => IO {
            trec.entries.get(tvar.id) match {
              case Some(v) => v.current.asInstanceOf[A]
              case None =>
                val v = tvar.a
                trec.insert(tvar, v)
                v
            }
          }
          case WriteTVar(tvar, v) => IO {
            trec.entries.get(tvar.id) match {
              case Some(entry) => entry.current = v
              case None => trec.insert(tvar, v)
            }
          }
        }
      }
    }

  def atomically[A]: STM[A] => IO[A] = p =>
    interpreter.flatMap(p.foldMap(_))


  def yo = for {
    v <- newTVar(1)
    a <- readTVar(v)
    _ <- writeTVar(v, 2)
    b <- readTVar(v)
    c = v.a
  } yield (a, b, c)

  def p = atomically(yo).unsafeRunSync

}
