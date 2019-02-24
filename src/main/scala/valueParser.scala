import scala.language.{higherKinds, implicitConversions}

trait MatchValue[V[_], A] {
  def matched(endingBefore: NonNegativeInt, value: A): V[A]
  def mismatched: V[A]
}

trait ValueResult[A] {
  def apply[V[_]](mv: MatchValue[V, A]): V[A]
}

object ValueResult {
  def wasMatch[A](endingBefore: NonNegativeInt, value: A): ValueResult[A] = new {
    def apply[V[_]](mv: MatchValue[V, A]) = mv.matched(endingBefore, value)
  }
  def wasMismatch[A]: ValueResult[A] = new {
    def apply[V[_]](mv: MatchValue[V, A]) = mv.mismatched
  }
}

opaque type Value[Buffer, A] = (Buffer, NonNegativeInt) => ValueResult[A]

object Value {
  import Position._

  def capture[Buffer, Token](p: Position[Buffer]) given TokenBuffer[Buffer, Token]: Value[Buffer, Buffer] =
  (buff, pos) =>
    p(buff, pos)(new {
      override def matched(end: NonNegativeInt) = ValueResult.wasMatch(end, buff.subBuffer(pos, end))
      override def mismatched = ValueResult.wasMismatch
    })

  def (p: Value[Buffer, A]) map[Buffer, A, B](f: A => B): Value[Buffer, B] =
  (buff, pos) => p(buff, pos)(new MatchValue[Const[ValueResult[B]], A] {
    override def matched(end: NonNegativeInt, value: A) = ValueResult.wasMatch(end, f(value))
    override def mismatched = ValueResult.wasMismatch
  })

  implied ValueAndThenPosition[Buffer, A] for ParseOneThenOther[Value[Buffer, A], Position[Buffer], Value[Buffer, A]] =
    (lhs, rhs) => (buff, pos) => lhs(buff, pos)(new {
      override def matched(lEnd: NonNegativeInt, value: A) = rhs(buff, lEnd)(new {
        override def matched(rEnd: NonNegativeInt) = ValueResult.wasMatch(rEnd, value)
        override def mismatched = ValueResult.wasMismatch
      })
      override def mismatched = ValueResult.wasMismatch
    })

  implied PositionAndThenValue[Buffer, B] for ParseOneThenOther[Position[Buffer], Value[Buffer, B], Value[Buffer, B]] =
    (lhs, rhs) => (buff, pos) => lhs(buff, pos)(new {
      override def matched(lEnd: NonNegativeInt) = rhs(buff, lEnd)(new {
        override def matched(rEnd: NonNegativeInt, value: B) = ValueResult.wasMatch(rEnd, value)
        override def mismatched = ValueResult.wasMismatch
      })
      override def mismatched = ValueResult.wasMismatch
    })

  implied PositionAndThenPosition[Buffer, A, B] for ParseOneThenOther[Value[Buffer, A], Value[Buffer, B], Value[Buffer, (A, B)]] =
    (lhs, rhs) => (buff, pos) => lhs(buff, pos)(new MatchValue[Const[ValueResult[(A, B)]], A] {
      override def matched(lEnd: NonNegativeInt, lValue: A): ValueResult[(A, B)] = {
        rhs(buff, lEnd)(new MatchValue[Const[ValueResult[(A, B)]], B] {
          override def matched(rEnd: NonNegativeInt, rValue: B): ValueResult[(A, B)] = ValueResult.wasMatch(rEnd, (lValue, rValue))
          override def mismatched: ValueResult[(A, B)] = ValueResult.wasMismatch
        })
      }
      override def mismatched: ValueResult[(A, B)] = ValueResult.wasMismatch
    })


  implied RunValue[Buffer, A] for RunDSL[Value[Buffer, A], Buffer => ValueResult[A]] = p => p(_, NonNegativeInt(0))
}

implied [Buffer, A] for RunDSL[Value[Buffer, A], Buffer => ValueResult[A]] = Value.RunValue

//implied ValueSyntax {
//  def (p: Position[Buffer]) capture[Buffer, Token] given TokenBuffer[Buffer, Token]: Value[Buffer, Buffer] =
//    Value.capture(p)
//}

