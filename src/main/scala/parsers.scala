import scala.language.{higherKinds, implicitConversions}

/** Non-negative integers.
  * n >= 0
  */
opaque type NonNegativeInt = Int // >= 0
object NonNegativeInt {
  def apply(i: Int): NonNegativeInt =
    if(i >= 0) i else throw new IllegalArgumentException(s"Expecting a non-negative integer but got $i")

  // ignores overflow
  def (lhs: NonNegativeInt) + (rhs: NonNegativeInt): NonNegativeInt = lhs + rhs

  implied NNFromInt for Conversion[Int, NonNegativeInt] = apply
  implied IntFromNN for Conversion[NonNegativeInt, Int] = identity
}

// something isn't working right - I shouldn't need to import this
import NonNegativeInt.+

/** Positive integers.
  * N > 0
  */
opaque type PositiveInt = Int
object PositiveInt {
  def apply(i: Int): PositiveInt =
    if(i > 0) i else throw new IllegalArgumentException(s"Expecting a positive intenger but got $i")

  implied PIFromInt for Conversion[Int, PositiveInt] = apply
  implied IntFromPI for Conversion[PositiveInt, Int] = identity
}



trait CombineParserValue[S, T, U] {
  def combine(s: S, t: T): U
}

object CombineParserValue {
  implied CombineUnits        for CombineParserValue[Unit, Unit,  Unit ]  = (_, _) => {}
  implied CombineLeftUnit[T]  for CombineParserValue[Unit, T,     T    ]  = (_, t) => t
  implied CombineRightUnit[S] for CombineParserValue[S,    Unit,  S    ]  = (s, _) => s
  implied CombineTupled[S, T] for CombineParserValue[S,    T,    (S, T)]  = (s, t) => ((s, t))
}


trait AlternativeParserValue[S, T, U] {
  def left(s: S): U
  def right(t: T): U
}

object AlternativeParserValue {
  /** (P[S] orAlternatively P[S]) : P[S] */
  implied AlternativeIdentity[S] for AlternativeParserValue[S, S, S] {
    override def left(s: S) = s
    override def right(s: S) = s
  }

  implied AlternativeLeftSupertype[S, T] given (st: T <:< S) for AlternativeParserValue[S, T, S] {
    override def left(s: S) = s
    override def right(t: T) = st(t)
  }

  implied AlternativeRightSupertype[S, T] given (st: S <:< T) for AlternativeParserValue[S, T, T] {
    override def left(s: S) = st(s)
    override def right(t: T) = t
  }
}

// Generalised base cases and combinators for parsers.
trait Parser[R[_]] {
  /** The parser that consumes no input and always succeeds */
  def succeed[T]: R[T]

  /** The parser that consumes no input and always fails */
  def fail[T]: R[T]

  /** Combine two parsers so that the first one will attempt to match, and if it does, the second one will.
    * The combined parser will require both to match, or fail.
    */
  def (lhs: R[S]) andThen[S, T, U] (rhs: R[T]) given CombineParserValue[S, T, U]: R[U] // semigroup-like, non-symmetrical as consumes left-to-right

  /** Combine two parsers so that the first one is tried, and if it fails, the second one is tried.
    * The combined parser will succeed if either do, or fail if both fail.
    */
  def (lhs: R[S]) orAlternatively[S, T, U] (rhs: R[T]) given AlternativeParserValue[S, T, U]: R[U] // semigroup, non-symmetrical as first alternative takes prescedence if rhs intersects lhs

  /** Attempt to repeatedly apply a parser, potentially interspersed with a separator, potentially with minimum and
    * maximum repetitions.
    */
  def (lhs: R[S]) repeatedly[S, T, C] (
    sep: R[T] = succeed,
    minTimes: NonNegativeInt = 0,
    maxTimes: PositiveInt = Integer.MAX_VALUE) given Postpend[C, S]: C
}




/** A parser that supports looking ahead into the input. */
trait LookaheadParser[R[_]] {
  /** Look ahead with a parser and succeed if it would match, without consuming input. */
  def positiveLookAhead[S] (lhs: R[S]): R[Unit]
  /** Look ahead with a parser and succeed if it would not match, without consuming input. */
  def negativeLookAhead[S] (lhs: R[S]): R[Unit]
}



/** A parser that can selectively parse individual tokens. */
trait TokenParser[Token, R[_]] {
  /** Match to any single token and advance by one index. */
  def anyToken: R[Unit]
  /** Match a single specified token. */
  def token(t: Token): R[Unit]
  // token('a') andThen token('b') ~~> tokens("ab")
  // token('a') andThen tokens("B") ~~> tokens("aB")
  // tokens("A") andThen token('b') ~~> tokens("Ab")
  // tokens("a") <~~> token('a')
  /** Match one for a set for tokens. */
  def anyOf(ts: Seq[Token]*): R[Unit]
  // token('a') | token('b') <~~> anyOf("ab")
  /** Match a token filtered by a predicate. */
  def forAny(p: Token => Boolean): R[Unit]
}


/** A parser that can efficiently match strings for tokens. */
trait TokenStringParser[Buffer, R[_]] {
  /** Match a sequence for tokens in-order. */
  def tokens(ts: Buffer): R[Unit]
  // tokens("") ~~> success
}



/**
  * A token buffer is an indexed collection for tokens, indexed from zero to length-1.
  *
  * It is expected, but not enforced, that lookup should be highly efficient.
  * Scanning over buffers is a core operation for efficient parser implementation.
  *
  * @tparam Buffer
  * @tparam Token
  */
trait TokenBuffer[Buffer, Token] {
  /** Buffer length. */
  def length(b: Buffer): NonNegativeInt

  // pos < length
  /** Fetch a token from a buffer by index. */
  def (b: Buffer) tokenAt (pos: NonNegativeInt): Token

  def (b: Buffer) span (startingFrom: NonNegativeInt, endingBefore: NonNegativeInt): Buffer
}

object TokenBuffer {

  implied CharSequenceTokenBuffer for TokenBuffer[CharSequence, Char] {
    override def (b: CharSequence) length: NonNegativeInt = b.length
    override def (b: CharSequence) tokenAt (pos: NonNegativeInt): Char = b.charAt(pos)
    override def (b: CharSequence) span (startingFrom: NonNegativeInt, endingBefore: NonNegativeInt): CharSequence =
    b.subSequence(startingFrom, endingBefore)
  }
}



trait MatchResult[M] {
  def matched(endingBefore: NonNegativeInt): M
  def mismatch: M
}

@FunctionalInterface
trait ParseResult {
  def apply[M](mr: MatchResult[M]): M
}

object ParseResult {
  def matched(endingBefore: NonNegativeInt): ParseResult = new {
    override def apply[M](mr: MatchResult[M]): M = mr.matched(endingBefore)
  }
  // object? val? def? what will be best for inlining/erasing?
  object mismatch extends ParseResult {
    override def apply[M](mr: MatchResult[M]): M = mr.mismatch
  }
}

opaque type ParsePosition[Buffer] = (Buffer, NonNegativeInt) => ParseResult

object ParsePosition {

  implied BufferTokenParser[Buffer, Token]
    given (TB: TokenBuffer[Buffer, Token], E: Equiv[Token]) for TokenParser[Token, Const[ParsePosition[Buffer]]] {
    override def anyToken: ParsePosition[Buffer] = (buff, pos) =>
      ParseResult.matched(pos + 1)

      override def token(t: Token): ParsePosition[Buffer] = (buff, pos) =>
      if(E.equiv(buff tokenAt pos, t)) ParseResult.matched(pos + 1)
      else ParseResult.mismatch

      override def anyOf(ts: Seq[Token]*) =
    {
      val tokens = Set.empty ++ ts.flatten
      (buff, pos) =>
        if (tokens contains (buff tokenAt pos)) ParseResult.matched(pos + 1)
        else ParseResult.mismatch
    }

    override def forAny(p: Token => Boolean) = (buff, pos) =>
      if(p(buff tokenAt pos)) ParseResult.matched(pos + 1)
      else ParseResult.mismatch
  }

}


//@FunctionalInterface
//trait ParseCtxt[Buffer] {
//  def parseFrom(b: Buffer, pos: PositiveInt, matching: MatchRange[NonNegativeInt]): NonNegativeInt
//}
//
//
//object foo {
//  implied Foo[Buffer, R] for Parser[ParseCtxt[Buffer]] {
//    override def succeed = (_, p, mr) => mr.matched(p)
//      overide def fail = (_, _, mr) => mr.mismatched
//    override def (lhs) andThen (rhs) =
//      (b, pos, m) => lhs(b, pos, new {
//        def matching(lhsPos) = rhs(b, lhsPos, new {
//          def matching(rhsPos) = m.matching(rhsPos)
//          def mismtach = m.mismatch
//        })
//        def mismatch = m.mismatch
//      })
//    override def (lhs) orAlternatively (rhs) =
//      (b, pos, m) =>
//        lhs(b, pos, new {
//          def matching(lhsPos) = m.matching(lhsPos)
//          def mismatch = rhs(b, pos, new {
//            rhs(b, pos, new {
//              def matching(rhsPos) = m.matching(rhsPos)
//              def mismatch = m.mismatch
//            })
//          })
//        })
//  }
//}
//
//

//trait JsonParser[R] {
//  def (l: R) ? = l.rep(min = 0, max = 1)
//  def (l: R) + = l.rep(min = 1)
//  def (l: R) * = l.rep
//  def (l: R) ~ (r: R) = l andThen r
//  def (l: R) | (r: R) = l orAlternatively r
//
//  def l ~~ r = l ~ whitespace.? ~ r
//
//  val exc: R = "\"
//  val qt: R = '"'
//  val onenine: R = anyOf('1' .. '9')
//  val digit: R = anyOf('0' .. '9')
//  val digits: R = digit.+
//  val hex = digit | anyOf('a'..'f') | anyOf('A'..'F')
//
//  lazy val jArray: R = "[" ~~ jValue.repeatedly(separatedBy = "," ~ whitespace.?) ~~ "]"
//  lazy val jObject: R = "{" ~~ jPropertyAssignment.repeatedly(separatedBy = ";") ~~ "}"
//  lazy val jProperty: R = jString ~~ ":" ~~ jValue
//  lazy val jString: R = qt ~ ((negativeLookAhead(qt | esc) ~ anyToken)) | (esc ~ (escCode | hex.+)) ~ qt
//  lazy val jNumber: R = jInt ~ frac ~ exp
//  lazy val jInt: R = "-".? ~ (digit | (onenine ~ digits))
//  lazy val jFrac: R = ("." ~ digits).?
//  lazy val jTrue: R = "true"
//  lazy val jFalse: R = "false"
//  lazy val jNull: R = "null"
//
//  lazy val jValue: R = jStringVal | jArray | jObject | jNumber | jTrue | jFalse | jNull
//}