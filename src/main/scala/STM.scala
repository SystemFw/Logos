
object stm {
  import cats._, implicits._
  import cats.free._
  import cats.effect._, concurrent._
  import scala.collection.mutable

  trait STM_[A]
  case class NewTVar[A](a: A) extends STM_[TVar[A]]
  case class ReadTVar[A](t: TVar[A]) extends STM_[A]
  case class WriteTVar[A](t: TVar[A], v: A) extends STM_[Unit]

  type STM[A] = Free[STM_, A]

  object STM {
    import scala.concurrent.ExecutionContext.Implicits.global
    val globalLock = Semaphore[IO](1).unsafeRunSync
  }

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

  class TVar[A] private[stm](
    @volatile private var _value: A,
    private[stm] val id: Token,
    @volatile private var _timestamp: Long
  ) {

    def timestamp = _timestamp

    def update(v: A): Unit = {
      _value = v
      _timestamp += 1
    }

    def value = _value
  }

  private[stm] final class Entry[A](tvar: TVar[A], timestamp: Long, @volatile private var current: A) {
    def isValid: Boolean = tvar.timestamp == timestamp
    def commit(): Unit = tvar.update(current)
    def value = current
    def write(a: A) = current = a
  }


  private[stm] final class TRec(val entries: mutable.Map[Token, Entry[Any]]) {
    def insert[A](tvar: TVar[A], v: A): Unit = {
      entries += tvar.id -> new Entry(tvar.asInstanceOf[TVar[Any]], tvar.timestamp, v)
      ()
    }

    def validate: Boolean =
      entries.forall(_._2.isValid)

    def commit(): Unit =
      entries.foreach(_._2.commit)

    // def rollback(): Unit = {
    //   entries.foreach {case (k, _) =>  entries -= k }
    //   println("rollback")
    // }
      
  }

  def interpreter(trec: TRec): STM_ ~> IO =
      new (STM_ ~> IO) {
        def apply[A](prog: STM_[A]): IO[A] = prog match {
          case NewTVar(v) => IO(new TVar(v, new Token, 0L))
          case ReadTVar(tvar) => IO {
            trec.entries.get(tvar.id) match {
              case Some(v) => v.value.asInstanceOf[A]
              case None =>
                val v = tvar.value
                trec.insert(tvar, v)
                v
            }
          }
          case WriteTVar(tvar, v) => IO {
            trec.entries.get(tvar.id) match {
              case Some(entry) => entry.write(v)
              case None => trec.insert(tvar, v)
            }
          }
        }
      }



  def atomically[A]: STM[A] => IO[A] = p => {
    def atomic: IO[A] = IO(new TRec(mutable.Map.empty)).flatMap { trec =>

      // IDEA: let the interpreter return a `trec => IO`, so that I can use the
      // the partial interpreter trick and foldMap the Free only once, and not at
      // every re-run
      p.foldMap(interpreter(trec)).flatMap { result =>
        STM.globalLock.withPermit {
          IO {
            val valid = trec.validate
            if(valid) trec.commit
            valid
          }
        }.ifM(
          result.pure[IO],
          atomic
        )
      }
    }

    atomic
  }

  def t1 = {
    import scala.concurrent.ExecutionContext.Implicits.global

    def prog(account: TVar[Int], amount: Int): STM[Unit] =
      for {
        v <- readTVar(account)
        nv = v - amount
        _ <- writeTVar(account, nv)
      } yield ()

    def runner = for {
      account <- atomically(newTVar(200))
      p1 <- atomically(prog(account, 100)).start
      p2 <- atomically(prog(account, 100)).start
      _ <- p1.join
      _ <- p2.join
      v <- atomically(readTVar(account))
    } yield v


    def tests =
      List.fill(100000)(runner).sequence

    tests.unsafeRunSync.forall(_ == 0)
  }

}
