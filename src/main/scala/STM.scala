
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

    override def toString = s"(TVAR: id: $id , value $value , timestamp $timestamp)"
  }

  private[stm] final class Entry[A](tvar: TVar[A], timestamp: Long, @volatile private var current: A) {
    def isValid: Boolean = tvar.timestamp == timestamp
    def commit(): Unit = tvar.update(current)
    def value = current
    def write(a: A) = current = a

    override def toString = s"tvar $tvar , value: $value, timestamp $timestamp"
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

    override def toString = entries.map { case (_, v) => v.toString}.mkString("\n")
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



  def atomically[A](s: String): STM[A] => IO[A] = p => {
    def atomic: IO[A] = IO(new TRec(mutable.Map.empty)).flatMap { trec =>

      // IDEA: let the interpreter return a `trec => IO`, so that I can use the
      // the partial interpreter trick and foldMap the Free only once, and not at
      // every re-run
      p.foldMap(interpreter(trec)).flatMap { result =>
        STM.globalLock.withPermit {
          IO {
            val valid = trec.validate
            println(s"$s: BEFORE trec: $trec, valid? $valid")
            if(valid) trec.commit
            println(s"$s: AFTER trec: $trec")
            valid
          }
        }.ifM(
          IO(println(s"$s: success $result \n\n")) *> result.pure[IO],
          IO(println(s"$s: abort \n\n")) *> atomic
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
      account <- atomically("var")(newTVar(200))
      p1 <- atomically("p1")(prog(account, 100)).start
      p2 <- atomically("p2")(prog(account, 100)).start
      _ <- p1.join
      _ <- p2.join
      v <- atomically("read")(readTVar(account))
      _ <- IO(println("------------------------------"))
    } yield ()//v


    def tests =
      List.fill(100000)(runner).sequence

    tests.unsafeRunSync.forall(_ == 0)
    //    runner.iterateUntil(_ != 0).unsafeRunSync
    //runner.unsafeRunSync
  }

}

// var: BEFORE trec: , valid? true
// var: AFTER trec: 
// var: success (TVAR: id: Token(798a4b0) , value 200 , timestamp 0) 


// p1: BEFORE trec: tvar (TVAR: id: Token(798a4b0) , value 200 , timestamp 0) , value: 100, timestamp 0, valid? true
// p1: AFTER trec: tvar (TVAR: id: Token(798a4b0) , value 100 , timestamp 1) , value: 100, timestamp 0
// p1: success () 

// since value is 100, it means it had read 200 as the initial value, but somehow with a timestamp of 1
// there definitely is a race between reading from one fiber and committing in another
// can it be solved without locking?
// race in readTVar:
//  case None =>
//   val v = tvar.value
//   trec.insert(tvar, v)
//    |
//    ---   another fiber commits here and changes the timestamp: new timestamp for old value: BUG
//    | entries += tvar.id -> new Entry(tvar.asInstanceOf[TVar[Any]], tvar.timestamp, v)
//    |
//   v


// p2: BEFORE trec: tvar (TVAR: id: Token(798a4b0) , value 100 , timestamp 1) , value: 100, timestamp 1, valid? true
// p2: AFTER trec: tvar (TVAR: id: Token(798a4b0) , value 100 , timestamp 2) , value: 100, timestamp 1
// p2: success () 


// read: BEFORE trec: tvar (TVAR: id: Token(798a4b0) , value 100 , timestamp 2) , value: 100, timestamp 2, valid? true
// read: AFTER trec: tvar (TVAR: id: Token(798a4b0) , value 100 , timestamp 3) , value: 100, timestamp 2
// read: success 100 
