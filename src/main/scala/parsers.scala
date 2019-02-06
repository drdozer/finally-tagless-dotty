//import Parser._
//
//trait ParserValueCombinator[S, T, U] {
//  def combine(s: S, t: T): U
//}
//
//object ParserValueCombinator {
//  instance CombineUnits of ParserValueCombinator[Unit, Unit, Unit] = (_, _) => {}
//  instance CombineLeftUnit[T] of ParserValueCombinator[Unit, T, T] = (_, t) => t
//  instance CombineRightUnit[S] of ParserValueCombinator[S, Unit, S] = (s, _) => s
//  instance CombineTupled[S, T] of ParserValueCombinator[S, T] = identity // (s, t) => (s, t)
//}
//
//// Generalised base cases and combinators for parsers.
//trait Parser[R[+_]] {
//  /** The parser that consumes no input and always succeeds */
//  def succeed[T]: R[T]
//
//  /** The parser that consumes no input and always fails */
//  def fail[T]: R[T]
//
//  /** Combine two parsers so that the first one will attempt to match, and if it does, the second one will.
//    * The combined parser will require both to match, or fail.
//    */
//  def (lhs: R[S])[S, T, U] andThen (rhs: R[T]): R[U] with ParserValueCombinator[S, T, U] // semigroup-like, non-symmetrical as consumes left-to-right
//
//  /** Combine two parsers so that the first one is tried, and if it fails, the second one is tried.
//    * The combined parser will succeed if either do, or fail if both fail.
//    */
//  def (lhs: R[S])[S, T, U] orAlternatively (rhs: R[T]): R[U]    // semigroup, non-symmetrical as first alternative takes prescedence if rhs intersects lhs
//
//  /** Attempt to repeatedly apply a parser, potentially interspersed with a separator, potentially with minimum and
//    * maximum repetitions.
//    */
////  def (lhs: R) repeatedly (sep: R = succeed, minTimes: NonNegativeInt = 0, maxTimes: PositiveInt = Integer.MAX_VALUE)
//}
//
//
///**
//  * Some generic parser uitilities.
//  */
//object Parser {
//  /** Non-negative integers.
//    * n >= 0
//    */
//  opaque type NonNegativeInt = Int // >= 0
//  object NonNegativeInt {
//    def apply(i: Int): NonNegativeInt =
//      if(i >= 0) i else throw new IllegalArgumentException(s"Expecting a non-negative integer but got $i")
//
//    instance NNFromInt of Converter[Int, NonNegativeInt] { override def apply(i: Int): NonNegativeInt = NonNegativeInt(i) }
//    instance IntFromNN of Converter[NonNegativeInt, Int] { override def apply(n: NonNegativeInt): Int = n }
//  }
//
//  /** Positive integers.
//    * N > 0
//    */
//  opaque type PositiveInt = Int
//  object PositiveInt {
//    def apply(i: Int): PositiveInt =
//      if(i > 0) i else throw new IllegalArgumentException(s"Expecting a positive intenger but got $i")
//
//    instance PIFromInt of Converter[Int, PositiveInt] { override def apply(i: Int): PositiveInt = PositiveInt(i))}
//    instance IntFromPI of Converter[PositiveInt, Int] { override def apply(n: PositiveInt): Int = n }
//  }
//}
//
//
///** A parser that supports looking ahead into the input. */
//trait LookaheadParser[R] {
//  /** Look ahead with a parser and succeed if it would match, without consuming input. */
//  def positiveLookAhead (lhs: R): R
//  /** Look ahead with a parser and succeed if it would not match, without consuming input. */
//  def negativeLookAhead (lhs: R): R
//}
//
//
///**
//  * A token buffer is an indexed collection of tokens, indexed from zero to length-1.
//  *
//  * It is expected, but not enforced, that lookup should be highly efficient.
//  * Scanning over buffers is a core operation of efficient parser implementation.
//  *
//  * @tparam Buffer
//  * @tparam Token
//  */
//trait TokenBuffer[Buffer, Token] {
//  /** Buffer length. */
//  def length(b: Buffer): NonNegativeInt
//
//  // pos < length
//  /** Fetch a token from a buffer by index. */
//  def (b: Buffer) tokenAt (pos: NonNegativeInt): Token
//
//  def (b: Buffer) span (startingFrom: NonNegativeInt, endingBefore: NonNegativeInt): Buffer
//}
//
//object TokenBuffer {
//
//  instance CharSequenceTokenBuffer of TokenBuffer[CharSequence, Char] {
//    override def length(b: CharSequence): Char = b.length
//    override def (b: CharSequence) tokenAt (pos: NonNegativeInt): Char = b.charAt(pos)
//    override def (b: CharSequence) span (startingFrom: NonNegativeInt, endingBefore: NonNegativeInt): CharSequence = b.subSequence(startingFrom, endingBefore)
//  }
//
//}
//
//
///** A parser that can selectively parse individual tokens. */
//trait TokenParser[Token, R] {
//  /** Match to any single token and advance by one index. */
//  def anyToken: R
//  /** Match a single specified token. */
//  def token(t: Token): R
//  // token('a') andThen token('b') ~~> tokens("ab")
//  // token('a') andThen tokens("B") ~~> tokens("aB")
//  // tokens("A") andThen token('b') ~~> tokens("Ab")
//  // tokens("a") <~~> token('a')
//  /** Match one of a set of tokens. */
//  def anyOf(ts: Seq[Token]*): R
//  // token('a') | token('b') <~~> anyOf("ab")
//  /** Match a token filtered by a predicate. */
//  def forAny(p: Token => Boolean): R
//}
//
//
///** A parser that can efficiently match strings of tokens. */
//trait TokenStringParser[Buffer, R] {
//  /** Match a sequence of tokens in-order. */
//  def tokens(ts: Buffer): R
//  // tokens("") ~~> success
//}
//
//
//
//trait MatchRange[M] {
//  def matched(endingBefore: NonNegativeInt): M
//  def mismatch: M
//}
//
//@FunctionalInterface
//trait ParseCtxt[Buffer] {
//  def parseFrom(b: Buffer, pos: PositiveInt, matching: MatchRange[NonNegativeInt]): NonNegativeInt
//}
//
//
//object foo {
//  instance Foo[Buffer, R] of Parser[ParseCtxt[Buffer]] {
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