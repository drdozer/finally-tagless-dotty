import scala.language.{higherKinds, implicitConversions}

/** Non-negative integers.
  * n >= 0
  */
opaque type NonNegativeInt = Int // >= 0
object NonNegativeInt {
  inline def apply(i: Int): NonNegativeInt =
    if(i >= 0) i else throw new IllegalArgumentException(s"Expecting a non-negative integer but got $i")

  // ignores overflow
  def (lhs: NonNegativeInt) + (rhs: Int): NonNegativeInt = apply(lhs + rhs)

//  implied NNFromInt for Conversion[Int, NonNegativeInt] = apply
  implied IntFromNN for Conversion[NonNegativeInt, Int] = identity
}

import NonNegativeInt.+

// something isn't working right - I shouldn't need to import this

/** Positive integers.
  * N > 0
  */
opaque type PositiveInt = Int
object PositiveInt {
  inline def apply(i: Int): PositiveInt =
    if(i > 0) i else throw new IllegalArgumentException(s"Expecting a positive intenger but got $i")

//  implied PIFromInt for Conversion[Int, PositiveInt] = apply
  implied IntFromPI for Conversion[PositiveInt, Int] = identity
}



/** Generalised base cases for parsers.
  *
  * @tparam R   Parser representation type
  */
trait Parser[R] {
  /** The parser that consumes no input and always succeeds */
  def succeed: R

  /** The parser that consumes no input and always fails */
  def fail: R
}




/** A parser that supports looking ahead into the input. */
trait LookaheadParser[R] {
  /** Look ahead with a parser and succeed if it would match, without consuming input. */
  def positiveLookAhead (lhs: R): R
  /** Look ahead with a parser and succeed if it would not match, without consuming input. */
  def negativeLookAhead (lhs: R): R
}


/** A parser that matches at key landmarks within the input. */
trait ParserLocations[R] {
  /** Matches only at the very beginning of the input, before any input has been accepted. */
  def beginning: R

  /** Matches only at the very end of the input, after all input has been accepted. */
  def ending: R
}



/** A parser that can selectively parse individual tokens. */
trait TokenParser[Token, R] {
  /** Match to any single token and advance by one index. */
  def anyToken: R
  /** Match a single specified token. */
  def token(t: Token): R
  // token('a') andThen token('b') ~~> tokens("ab")
  // token('a') andThen tokens("B") ~~> tokens("aB")
  // tokens("A") andThen token('b') ~~> tokens("Ab")
  // tokens("a") <~~> token('a')
  /** Match one for a set for tokens. */
  def anyOf(ts: Seq[Token]*): R
  // token('a') | token('b') <~~> anyOf("ab")
  /** Match a token filtered by a predicate. */
  def forAny(p: Token => Boolean): R
}




/** A parser that can efficiently match strings for tokens. */
trait TokenStringParser[Buffer, R] {
  /** Match a sequence for tokens in-order. */
  def tokens(ts: Buffer): R
  // tokens("") ~~> success
}



/** A parser combinator that combines two parsers to match one then the other on the input.
  *
  * @tparam P   the type of the first parser to try
  * @tparam Q   the type of the second parser to try
  * @tparam R   the type of the parser representing applying `P` and `Q` in turn
  */
trait ParseOneThenOther[P, Q, R] {
  /** Combine two parsers so that the first one will attempt to match, and if it does, the second one will.
    * The combined parser will require both to match, or fail.
    */
  def (lhs: P) andThen (rhs: Q): R
}



/** A parser combinator that combines two parsers one or the other on the input.
  *
  * @tparam P   the type of the first parser to try
  * @tparam Q   the type of the second parser to try
  * @tparam R   the type of the parser representing applying `P` or `Q`
  */
trait ParseOneOrOther[P, Q, R] {
  /** Combine two parsers so that the first one is tried, and if it fails, the second one is tried.
    * The combined parser will succeed if either do, or fail if both fail.
    */
  def (lhs: P) orAlternatively (rhs: Q): R
}



trait ParseRepeatedly[P, Q, R] {

  /** Attempt to repeatedly apply a parser, potentially interspersed with a separator, potentially with minimum and
    * maximum repetitions.
    */
  def (lhs: P) repeatedly(
    sep: Q,
    minTimes: NonNegativeInt = NonNegativeInt(0),
    maxTimes: PositiveInt = PositiveInt(Integer.MAX_VALUE)): R
}


object ParseRepeatedly {
  implied DefaultParseRepeatedly[P, Q, R] for ParseRepeatedly[P, Q, R] {
    override def (lhs: P) repeatedly(
      sep: Q,
      minTimes: NonNegativeInt = NonNegativeInt(0),
      maxTimes: PositiveInt = PositiveInt(Integer.MAX_VALUE)): R = {

      def whileMin(min: NonNegativeInt) =
        if(min == 0) untilMax(maxTimes - minTimes)
        else lhs andThen sep andThen whileMin(min - 1)

      def untilMax(max: PositiveInt) =
        lhs andThen (
          if max == 0 success
          else sep andThen untilMax(max - 1) orAlternatively success
        )

      whileMin(minTimes, maxTimes)
    }

  }
}


/** A parser that captures input.
  *
  * @tparam Buffer  the type of the input captured
  * @tparam P       the underlying parser type
  * @tparam Q       the parser type representing captured input
  */
trait ParserCapture[Buffer, P, Q[_]] {
  /** Capture the input matched by a parser as a buffer, presumably the same buffer type as the parser was parsing. */
  def (p: P) capture: Q[Buffer]
}
