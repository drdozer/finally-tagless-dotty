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



trait MatchPosition[M] {
  def matched(endingBefore: NonNegativeInt): M
  def mismatched: M
}


trait PositionResult {
  def apply[M](mr: MatchPosition[M]): M
}

object PositionResult {
  def wasMatch(endingBefore: NonNegativeInt): PositionResult = new {
    override def apply[M](mr: MatchPosition[M]): M = mr.matched(endingBefore)
  }
  // object? val? def? what will be best for inlining/erasing?
  object wasMismatch extends PositionResult {
    override def apply[M](mr: MatchPosition[M]): M = mr.mismatched
  }

  def handleMatch(m: NonNegativeInt => NonNegativeInt = identity): MatchPosition[PositionResult] = new {
    override def matched(endingBefore: NonNegativeInt) = wasMatch(m(endingBefore))
    override def mismatched = wasMismatch
  }

  def matchIf(f: NonNegativeInt => Boolean, m: NonNegativeInt => NonNegativeInt = identity): MatchPosition[PositionResult] = new {
    override def matched(endingBefore: NonNegativeInt) =
      if(f(endingBefore)) wasMatch(m(endingBefore))
      else wasMismatch
    override def mismatched = wasMismatch
  }
}

opaque type Position[Buffer] = (Buffer, NonNegativeInt) => PositionResult

object Position {

  implied PositionParser[Buffer] for Parser[Position[Buffer]] {
    override def succeed: Position[Buffer] = (buff, pos) => PositionResult.wasMatch(pos)
    override def fail: Position[Buffer] = (buff, pos) => PositionResult.wasMismatch
  }

  implied PositionLookaheadParser[Buffer] for LookaheadParser[Position[Buffer]] {
    override def positiveLookAhead(lhs: Position[Buffer]): Position[Buffer] = (buff, pos) =>
      lhs(buff, pos)(PositionResult.handleMatch(_ => pos))

    override def negativeLookAhead(lhs: Position[Buffer]): Position[Buffer] = (buff, pos) => lhs(buff, pos)(new {
      override def matched(endingBefore: NonNegativeInt) = PositionResult.wasMismatch
      override def mismatched = PositionResult.wasMatch(pos)
    })
  }

  implied PositionParserLocations[Buffer, Token]
    given (TB: TokenBuffer[Buffer, Token]) for ParserLocations[Position[Buffer]] {
    override def beginning = (buff, pos) =>
      if(pos == NonNegativeInt(0)) PositionResult.wasMatch(pos)
      else PositionResult.wasMismatch
    override def ending = (buff, pos) =>
      if(pos == buff.length) PositionResult.wasMatch(pos)
      else PositionResult.wasMismatch
  }

  implied PositionTokenParser[Buffer, Token]
    given (TB: TokenBuffer[Buffer, Token], E: Equiv[Token]) for TokenParser[Token, Position[Buffer]] {
    override def anyToken: Position[Buffer] = (buff, pos) =>
      PositionResult.wasMatch(pos + 1)

      override def token(t: Token): Position[Buffer] = (buff, pos) =>
      if(E.equiv(buff tokenAt pos, t)) PositionResult.wasMatch(pos + 1)
      else PositionResult.wasMismatch

      override def anyOf(ts: Seq[Token]*) =
    {
      val tokens = Set.empty ++ ts.flatten
      (buff, pos) =>
        if (tokens contains (buff tokenAt pos)) PositionResult.wasMatch(pos + 1)
        else PositionResult.wasMismatch
    }

    override def forAny(p: Token => Boolean) = (buff, pos) =>
      if(p(buff tokenAt pos)) PositionResult.wasMatch(pos + 1)
      else PositionResult.wasMismatch
  }

  implied PositionBufferParser[Buffer, Token]
    given (TB: TokenBuffer[Buffer, Token], E: Equiv[Token]) for BufferParser[Buffer, Position[Buffer]] {
    override def tokens(ts: Buffer) = (buff, pos) => {
      val tsl = ts.length
      val bfl = buff.length
      def test(bi: NonNegativeInt, ti: NonNegativeInt): PositionResult =
        if(bi >= bfl) PositionResult.wasMismatch // run off the end of the input
        else if (ti >= tsl) PositionResult.wasMatch(bi) // fully matched input
        else if (E.equiv(buff tokenAt bi, ts tokenAt ti)) test(bi + 1, ti + 1) // may need optimisations here for bounds checks
        else PositionResult.wasMismatch // tokens differ

      test(pos, NonNegativeInt(0))
    }
  }

  implied PositionParseOneThenOther[Buffer] for ParseOneThenOther[Position[Buffer], Position[Buffer], Position[Buffer]] {
    override def andThen(lhs: Position[Buffer], rhs: Position[Buffer]): Position[Buffer] = (buff, pos) =>
      lhs(buff, pos)(new {
        override def matched(lhsEnd: NonNegativeInt) = rhs(buff, lhsEnd)(new {
          override def matched(rhsEnd: NonNegativeInt) = PositionResult.wasMatch(rhsEnd)
          override def mismatched = PositionResult.wasMismatch
        })
        override def mismatched = PositionResult.wasMismatch
      })
  }

  implied PositionParseOneOrOther[Buffer] for ParseOneOrOther[Position[Buffer], Position[Buffer], Position[Buffer]] {
    override def (lhs: Position[Buffer]) orAlternatively (rhs: Position[Buffer]): Position[Buffer] = (buff, pos) =>
      lhs(buff, pos)(new {
        override def matched(lhsEnd: NonNegativeInt) = PositionResult.wasMatch(lhsEnd)
        override def mismatched = rhs(buff, pos)(new {
          override def matched(rhsEnd: NonNegativeInt) = PositionResult.wasMatch(rhsEnd)
          override def mismatched = PositionResult.wasMismatch
        })
      })
  }
}


//trait JsonParser[R] {
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