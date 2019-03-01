import scala.language.{higherKinds, implicitConversions}

type ParseValue[Buffer, A] = Parse[Buffer, NonNegativeInt, (NonNegativeInt, A)]

object Value {
  import Parse._

  // type hint required for the MatchValue instance, to rewrite the return value to B
  implied ValueMapper[Buffer] for Mappable[[A] => ParseValue[Buffer, A]] {
    def (p: ParseValue[Buffer, A]) map[A, B](f: A => B): ParseValue[Buffer, B] =
    (buff, pos) => p(buff, pos)(new {
      override def matched = (end, value) => Result.wasMatch(end, f(value))
      override def mismatched = Result.wasMismatch
    })
  }


  implied ValueAndThenPosition[Buffer, A] for ParseOneThenOther[ParseValue[Buffer, A], ParseAt[Buffer], ParseValue[Buffer, A]] =
    (lhs, rhs) => (buff, pos) => lhs(buff, pos)(new {
      override def matched = (lEnd, value) => rhs(buff, lEnd)(new {
        override def matched = Result.wasMatch(_, value)
        override def mismatched = Result.wasMismatch
      })
      override def mismatched = Result.wasMismatch
    })


  implied PositionAndThenValue[Buffer, B] for ParseOneThenOther[ParseAt[Buffer], ParseValue[Buffer, B], ParseValue[Buffer, B]] =
    (lhs, rhs) => (buff, pos) => lhs(buff, pos)(new {
      override def matched = rhs(buff, _)(new {
        override def matched = (rEnd, value) => Result.wasMatch(rEnd, value)
        override def mismatched = Result.wasMismatch
      })
      override def mismatched = Result.wasMismatch
    })


  // type hints required ont he MatchValue instances as we're combining two values, meaning dotty would have to work out two types with holes
  implied PositionAndThenPosition[Buffer, A, B] for ParseOneThenOther[ParseValue[Buffer, A], ParseValue[Buffer, B], ParseValue[Buffer, (A, B)]] =
    (lhs, rhs) => (buff, pos) => lhs(buff, pos)(new {
      override def matched = (lEnd, lValue) =>
        rhs(buff, lEnd)(new {
          override def matched = (rEnd, rValue) => Result.wasMatch(rEnd, (lValue, rValue))
          override def mismatched = Result.wasMismatch
        })
      override def mismatched = Result.wasMismatch
    })


  implied ValueOrAlternativelyValue[Buffer, A] for ParseOneOrOther[ParseValue[Buffer, A], ParseValue[Buffer, A], ParseValue[Buffer, A]] =
    (lhs, rhs) => (buff, pos) => lhs(buff, pos)(new {
      override def matched = (end, value) => Result.wasMatch(end, value)
      override def mismatched = rhs(buff, pos)(new {
        override def matched = (end, value) => Result.wasMatch(end, value)
        override def mismatched = Result.wasMismatch
      })
    })

  implied RunValue[Buffer, A] for RunDSL[ParseValue[Buffer, A], Buffer => Result[(NonNegativeInt, A)]] = p => p(_, NonNegativeInt(0))
}

implied [Buffer, A] for RunDSL[ParseValue[Buffer, A], Buffer => Result[(NonNegativeInt, A)]] = Value.RunValue
