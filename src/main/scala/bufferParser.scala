import ct._

import scala.language.implicitConversions
import NonNegativeInt.+

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
  def (b: Buffer) length: NonNegativeInt

  // pos < length
  /** Fetch a token from a buffer by index. */
  def (b: Buffer) tokenAt (pos: NonNegativeInt): Token

  def (b: Buffer) subBuffer (startingFrom: NonNegativeInt, endingBefore: NonNegativeInt): Buffer
}

implied CharSequenceTokenBuffer for TokenBuffer[CharSequence, Char] {
  override def (b: CharSequence) length: NonNegativeInt = NonNegativeInt(b.length)
  override def (b: CharSequence) tokenAt (pos: NonNegativeInt): Char = b.charAt(pos)
  override def (b: CharSequence) subBuffer (startingFrom: NonNegativeInt, endingBefore: NonNegativeInt): CharSequence =
  b.subSequence(startingFrom, endingBefore)
}



trait ParseResult[S, M] {
  def matched: S => M
  def mismatched: M
}

object ParseResult {
  def apply[S, M](matched: S => M, mismatched: M): ParseResult[S, M] = {
    val ma = matched
    val mi = mismatched
    new {
      override def matched = ma
      override def mismatched = mi
    }
  }
}


trait Result[S] {
  def apply[M](mr: ParseResult[S, M]): M
}

object Result {
  def wasMatch[S](s: S): Result[S] = new {
    override def apply[M](mr: ParseResult[S, M]): M = mr.matched(s)
  }
  // object? val? def? what will be best for inlining/erasing?
  def wasMismatch[S]: Result[S] = new {
    override def apply[M](mr: ParseResult[S, M]): M = mr.mismatched
  }

  def handleMatch(m: NonNegativeInt => NonNegativeInt = identity): ParseResult[NonNegativeInt, Result[NonNegativeInt]] = ParseResult(
    matched = endingBefore => wasMatch(m(endingBefore))
      ,
    mismatched = wasMismatch
  )

  def matchIf(f: NonNegativeInt => Boolean, m: NonNegativeInt => NonNegativeInt = identity): ParseResult[NonNegativeInt, Result[NonNegativeInt]] = ParseResult(
    matched = endingBefore =>
      if(f(endingBefore)) wasMatch(m(endingBefore))
      else wasMismatch
    ,
    mismatched = wasMismatch
  )
}

// todo: there's a dotty bug with type aliases for opaques
// todo: it works if we remove the `opaque` modifier from `Parse`
type Parse[Buffer, In, Out] = (Buffer, In) => Result[Out]

/** A parser that tracks the location through an input buffer. */
type ParseAt[Buffer] = Parse[Buffer, NonNegativeInt, NonNegativeInt]

/** A parser that tracks both a position and a result value. */
type ParseValue[Buffer, A] = Parse[Buffer, NonNegativeInt, (NonNegativeInt, A)]


implied PositionParser[Buffer] for Parser[ParseAt[Buffer]] {
  override def succeed = (buff, pos) => Result.wasMatch(pos)
    override def fail = (buff, pos) => Result.wasMismatch
}

implied PositionLookaheadParser[Buffer] for LookaheadParser[ParseAt[Buffer]] {
  override def positiveLookAhead(lhs: ParseAt[Buffer]) = (buff, pos) =>
    lhs(buff, pos)(Result.handleMatch(_ => pos))

    override def negativeLookAhead(lhs: ParseAt[Buffer]) = (buff, pos) => lhs(buff, pos)(ParseResult(
    matched = endingBefore => Result.wasMismatch,
    mismatched = Result.wasMatch(pos)
  ))
}

implied PositionParserLocations[Buffer, Token]
  given (TB: TokenBuffer[Buffer, Token]) for ParserLocations[ParseAt[Buffer]] {
  override def beginning = (buff, pos) =>
    if(pos == NonNegativeInt(0)) Result.wasMatch(pos)
    else Result.wasMismatch
  override def ending = (buff, pos) =>
    if(pos == buff.length) Result.wasMatch(pos)
    else Result.wasMismatch
}

implied PositionTokenParser[Buffer, Token]
  given (TB: TokenBuffer[Buffer, Token], E: Equiv[Token]) for TokenParser[Token, ParseAt[Buffer]] {
  override def anyToken = (buff, pos) =>
    Result.wasMatch(pos + 1)

    override def token(t: Token) = (buff, pos) =>
    if(pos < buff.length && E.equiv(buff tokenAt pos, t)) Result.wasMatch(pos + 1)
    else Result.wasMismatch

    override def anyOf(ts: Seq[Token]*) =
  {
    val tokens = Set.empty ++ ts.flatten
    (buff, pos) =>
      if (pos < buff.length && (tokens contains (buff tokenAt pos))) Result.wasMatch(pos + 1)
      else Result.wasMismatch
  }

  override def forAny(p: Token => Boolean) = (buff, pos) =>
    if(pos < buff.length && p(buff tokenAt pos)) Result.wasMatch(pos + 1)
    else Result.wasMismatch
}

implied PositionBufferParser[Buffer, Token]
  given (TB: TokenBuffer[Buffer, Token], E: Equiv[Token]) for BufferParser[Buffer, ParseAt[Buffer]] =
  (ts) => (buff, pos) => {
    val tsl = ts.length
    val bfl = buff.length
    def test(bi: NonNegativeInt, ti: NonNegativeInt): Result[NonNegativeInt] =
      if(bi >= bfl) Result.wasMismatch // run off the end of the input
      else if (ti >= tsl) Result.wasMatch(bi) // fully matched input
      else if (E.equiv(buff tokenAt bi, ts tokenAt ti)) test(bi + 1, ti + 1) // may need optimisations here for bounds checks
      else Result.wasMismatch // tokens differ

    test(pos, NonNegativeInt(0))
  }


implied PositionParseOneThenOther[Buffer] for ParseOneThenOther[ParseAt[Buffer], ParseAt[Buffer], ParseAt[Buffer]] =
  (lhs, rhs) => (buff, pos) =>
    lhs(buff, pos)(new {
      override def matched = rhs(buff, _)(new {
        override def matched = Result.wasMatch
        override def mismatched = Result.wasMismatch
      })
      override def mismatched = Result.wasMismatch
    })


implied PositionParseOneOrOther[Buffer] for ParseOneOrOther[ParseAt[Buffer], ParseAt[Buffer], ParseAt[Buffer]] =
  (lhs, rhs) => (buff, pos) =>
    lhs(buff, pos)(new {
      override def matched = Result.wasMatch
      override def mismatched = rhs(buff, pos)(new {
        override def matched = Result.wasMatch
        override def mismatched = Result.wasMismatch
      })
    })


implied RunPositionParser[Buffer] for RunDSL[ParseAt[Buffer], Buffer => Result[NonNegativeInt]] = p => p(_, NonNegativeInt(0))


implied CapturePositionAsValue[Buffer, Token]
  given TokenBuffer[Buffer, Token] for ParserCapture[Buffer, ParseAt[Buffer], [A] => ParseValue[Buffer, A]] {
  def (p: ParseAt[Buffer]) capture = { (buff, pos) =>
    p(buff, pos)(ParseResult(
      matched = end => Result.wasMatch(end, buff.subBuffer(pos, end)),
      mismatched = Result.wasMismatch
    ))
  }
}


// type hint required for the MatchValue instance, to rewrite the return value to B
implied ValueMapper[Buffer] for Mappable[[A] => ParseValue[Buffer, A]] {
  def (p: ParseValue[Buffer, A]) map[A, B](f: A => B): ParseValue[Buffer, B] =
  (buff, pos) => p(buff, pos)(ParseResult(
    matched = (end, value) => Result.wasMatch(end, f(value)),
    mismatched = Result.wasMismatch
  ))
}


implied ValueAndThenPosition[Buffer, A] for ParseOneThenOther[ParseValue[Buffer, A], ParseAt[Buffer], ParseValue[Buffer, A]] =
  (lhs, rhs) => (buff, pos) => lhs(buff, pos)(ParseResult(
    matched = (lEnd, value) => rhs(buff, lEnd)(ParseResult(
      matched = Result.wasMatch(_, value),
      mismatched = Result.wasMismatch
    )),
    mismatched = Result.wasMismatch
    ))


implied PositionAndThenValue[Buffer, B] for ParseOneThenOther[ParseAt[Buffer], ParseValue[Buffer, B], ParseValue[Buffer, B]] =
  (lhs, rhs) => (buff, pos) => lhs(buff, pos)(ParseResult(
    matched = rhs(buff, _)(ParseResult(
      matched = (rEnd, value) => Result.wasMatch(rEnd, value),
      mismatched = Result.wasMismatch
    )),
    mismatched = Result.wasMismatch
  ))


// type hints required ont he MatchValue instances as we're combining two values, meaning dotty would have to work out two types with holes
implied PositionAndThenPosition[Buffer, A, B] for ParseOneThenOther[ParseValue[Buffer, A], ParseValue[Buffer, B], ParseValue[Buffer, (A, B)]] =
  (lhs, rhs) => (buff, pos) => lhs(buff, pos)(ParseResult(
    matched = (lEnd, lValue) =>
      rhs(buff, lEnd)(ParseResult(
        matched = (rEnd, rValue) => Result.wasMatch(rEnd, (lValue, rValue)),
        mismatched = Result.wasMismatch
      )),
    mismatched = Result.wasMismatch
  ))


implied ValueOrAlternativelyValue[Buffer, A] for ParseOneOrOther[ParseValue[Buffer, A], ParseValue[Buffer, A], ParseValue[Buffer, A]] =
  (lhs, rhs) => (buff, pos) => lhs(buff, pos)(ParseResult(
    matched = (end, value) => Result.wasMatch(end, value),
    mismatched = rhs(buff, pos)(ParseResult(
      matched = (end, value) => Result.wasMatch(end, value),
      mismatched = Result.wasMismatch
    ))
  ))

implied RunValue[Buffer, A] for RunDSL[ParseValue[Buffer, A], Buffer => Result[(NonNegativeInt, A)]] =
  p => p(_, NonNegativeInt(0))
