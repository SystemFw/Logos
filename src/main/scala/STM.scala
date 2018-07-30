package stm

object Y {
  import cats._, implicits._
  import cats.free._
  import cats.effect._, concurrent._

  trait STM_[A]
  case class ReadTVar[A](t: TVar[A]) extends STM_[A]
  case class NewTVar[A](a: A) extends STM_[TVar[A]]

  type STM[A] = Free[STM_, A]

  /** Represents a unique identifier (using object equality). */
  private[stm] final class Token extends Serializable {
    override def toString: String = s"Token(${hashCode.toHexString})"
  }

  class TVar[A](
    var a: A,
    val id: Token
  )
  object TVar {
    def create[A](a: A): IO[TVar[A]] = IO {
      new TVar(a, new Token)
    }
      
  }

  // todo is hashcode influenced by mutable field?
  case class TRec(
    vars: Map[Token, TVar[_]],
    values: Map[Token, Any]
  )

  def newTVar[A](a: A): STM[TVar[A]] =
   Free.liftF[STM_, TVar[A]](NewTVar(a))

  def readTVar[A](t: TVar[A]): STM[A] =
    Free.liftF(ReadTVar(t))

  def interpreter(state: Ref[IO, TRec]): STM_ ~> IO = {
    def nt =  new (STM_ ~> IO) {
      def apply[A](prog: STM_[A]): IO[A] = prog match {
        case NewTVar(a) => TVar.create(a)
        case ReadTVar(tvar) => state.modify(readTVarImpl(tvar))
      }
    }

    def readTVarImpl[A](tvar: TVar[A]): TRec => (TRec, A) =
      trec => {
        // todo access to tvar fields is not in IO, fix?
        trec.values.get(tvar.id) match {
          case Some(v) => trec -> v.asInstanceOf[A]
          case None =>
            val v = tvar.a
            val id = tvar.id
            trec.copy(
              vars = trec.vars + (id -> tvar),
              values = trec.values + (id -> v)
            ) -> v.asInstanceOf[A]
          
        }
      }

    nt
  }


  def yo = for {
    v <- newTVar(1)
    a <- readTVar(v)
    b <- readTVar(v)
  } yield a + b

  def p = yo.foldMap(interpreter(Ref.unsafe[IO, TRec](TRec(Map.empty, Map.empty)))).unsafeRunSync

}
