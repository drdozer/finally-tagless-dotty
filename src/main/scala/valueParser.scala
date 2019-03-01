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

  def apply[Buffer, A](p: (Buffer, NonNegativeInt) => ValueResult[A]): Value[Buffer, A] = p


  // type hint required for the MatchValue instance, to rewrite the return value to B
  implied ValueMapper[Buffer] for Mappable[[A] => Value[Buffer, A]] {
    def (p: Value[Buffer, A]) map[A, B](f: A => B): Value[Buffer, B] =
    (buff, pos) => p(buff, pos)(new MatchValue[Const[ValueResult[B]], A] {
      override def matched(end: NonNegativeInt, value: A) = ValueResult.wasMatch(end, f(value))
      override def mismatched = ValueResult.wasMismatch
    })
  }


  implied ValueAndThenPosition[Buffer, A] for ParseOneThenOther[Value[Buffer, A], Position[Buffer], Value[Buffer, A]] =
    (lhs, rhs) => (buff, pos) => lhs(buff, pos)(new {
      override def matched(lEnd: NonNegativeInt, value: A) = rhs(buff, lEnd)(new {
        override def matched = ValueResult.wasMatch(_, value)
        override def mismatched = ValueResult.wasMismatch
      })
      override def mismatched = ValueResult.wasMismatch
    })


  implied PositionAndThenValue[Buffer, B] for ParseOneThenOther[Position[Buffer], Value[Buffer, B], Value[Buffer, B]] =
    (lhs, rhs) => (buff, pos) => lhs(buff, pos)(new {
      override def matched = rhs(buff, _)(new {
        override def matched(rEnd: NonNegativeInt, value: B) = ValueResult.wasMatch(rEnd, value)
        override def mismatched = ValueResult.wasMismatch
      })
      override def mismatched = ValueResult.wasMismatch
    })


  // type hints required ont he MatchValue instances as we're combining two values, meaning dotty would have to work out two types with holes
  implied PositionAndThenPosition[Buffer, A, B] for ParseOneThenOther[Value[Buffer, A], Value[Buffer, B], Value[Buffer, (A, B)]] =
    (lhs, rhs) => (buff, pos) => lhs(buff, pos)(new MatchValue[Const[ValueResult[(A, B)]], A] {
      override def matched(lEnd: NonNegativeInt, lValue: A) = {
        rhs(buff, lEnd)(new MatchValue[Const[ValueResult[(A, B)]], B] {
          override def matched(rEnd: NonNegativeInt, rValue: B) = ValueResult.wasMatch(rEnd, (lValue, rValue))
          override def mismatched = ValueResult.wasMismatch
        })
      }
      override def mismatched: ValueResult[(A, B)] = ValueResult.wasMismatch
    })


  implied ValueOrAlternativelyValue[Buffer, A] for ParseOneOrOther[Value[Buffer, A], Value[Buffer, A], Value[Buffer, A]] =
    (lhs, rhs) => (buff, pos) => lhs(buff, pos)(new {
      override def matched(end: NonNegativeInt, value: A) = ValueResult.wasMatch(end, value)
      override def mismatched = rhs(buff, pos)(new {
        override def matched(end: NonNegativeInt, value: A) = ValueResult.wasMatch(end, value)
        override def mismatched = ValueResult.wasMismatch
      })
    })

  implied RunValue[Buffer, A] for RunDSL[Value[Buffer, A], Buffer => ValueResult[A]] = p => p(_, NonNegativeInt(0))
}

implied [Buffer, A] for RunDSL[Value[Buffer, A], Buffer => ValueResult[A]] = Value.RunValue
