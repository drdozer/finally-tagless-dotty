
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

object TokenBuffer {

  implied CharSequenceTokenBuffer for TokenBuffer[CharSequence, Char] {
    override def (b: CharSequence) length: NonNegativeInt = NonNegativeInt(b.length)
    override def (b: CharSequence) tokenAt (pos: NonNegativeInt): Char = b.charAt(pos)
    override def (b: CharSequence) subBuffer (startingFrom: NonNegativeInt, endingBefore: NonNegativeInt): CharSequence =
    b.subSequence(startingFrom, endingBefore)
  }

}



trait MatchPosition[M] {
  def matched(endingBefore: NonNegativeInt): M
  def mismatch: M
}

@FunctionalInterface
trait PositionResult {
  def apply[M](mr: MatchPosition[M]): M
}

object PositionResult {
  def matched(endingBefore: NonNegativeInt): PositionResult = new {
    override def apply[M](mr: MatchPosition[M]): M = mr.matched(endingBefore)
  }
  // object? val? def? what will be best for inlining/erasing?
  object mismatch extends PositionResult {
    override def apply[M](mr: MatchPosition[M]): M = mr.mismatch
  }
}

opaque type Position[Buffer] = (Buffer, NonNegativeInt) => PositionResult

object ParsePosition {

  implied BufferParser[Buffer] for Parser[Position[Buffer]] {
    override def succeed = (buf, pos) => PositionResult.matched(pos)
    override def failure = (buf, pos) => PositionResult.mismatch
  }

  implied BufferParserLocations[Buffer, Token]
    given (TB: TokenBuffer[Buffer, Token]) for ParserLocations[Position[Buffer]] {
    override def beginning = (buff, pos) =>
      if(pos == NonNegativeInt(0)) PositionResult.matched(pos)
      else PositionResult.mismatch
    override def ending = (buff, pos) =>
      if(pos == buff.length) PositionResult.matched(pos)
      else PositionResult.mismatch
  }

  implied BufferTokenParser[Buffer, Token]
    given (TB: TokenBuffer[Buffer, Token], E: Equiv[Token]) for TokenParser[Token, Position[Buffer]] {
    override def anyToken: Position[Buffer] = (buff, pos) =>
      PositionResult.matched(pos + 1)

      override def token(t: Token): Position[Buffer] = (buff, pos) =>
      if(E.equiv(buff tokenAt pos, t)) PositionResult.matched(pos + 1)
      else PositionResult.mismatch

      override def anyOf(ts: Seq[Token]*) =
    {
      val tokens = Set.empty ++ ts.flatten
      (buff, pos) =>
        if (tokens contains (buff tokenAt pos)) PositionResult.matched(pos + 1)
        else PositionResult.mismatch
    }

    override def forAny(p: Token => Boolean) = (buff, pos) =>
      if(p(buff tokenAt pos)) PositionResult.matched(pos + 1)
      else PositionResult.mismatch
  }

  implied BufferTokenStringParser[Buffer, Token]
    given (TB: TokenBuffer[Buffer, Token], E: Equiv[Token]) for TokenStringParser[Buffer, Position[Buffer]] {
    override def tokens(ts: Buffer) = (buff, pos) => {
      val tsl = ts.length
      val bfl = buff.length
      def test(bi: NonNegativeInt, ti: NonNegativeInt): PositionResult =
        if(bi >= bfl) PositionResult.mismatch // run off the end of the input
        else if (ti >= tsl) PositionResult.matched(bi) // fully matched input
        else if (E.equiv(buff tokenAt bi, ts tokenAt ti)) test(bi + 1, ti + 1) // may need optimisations here for bounds checks
        else PositionResult.mismatch // tokens differ

      test(pos, NonNegativeInt(0))
    }
  }

}


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