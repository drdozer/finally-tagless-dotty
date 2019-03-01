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


trait Result[S] {
  def apply[M](mr: ParseResult[S, M]): M
}

object Result {
  def wasMatch(endingBefore: NonNegativeInt): Result[NonNegativeInt] = new {
    override def apply[M](mr: ParseResult[NonNegativeInt, M]): M = mr.matched(endingBefore)
  }
  // object? val? def? what will be best for inlining/erasing?
  object wasMismatch extends Result[NonNegativeInt] {
    override def apply[M](mr: ParseResult[NonNegativeInt, M]): M = mr.mismatched
  }

  def handleMatch(m: NonNegativeInt => NonNegativeInt = identity): ParseResult[NonNegativeInt, Result[NonNegativeInt]] = new {
    override def matched = endingBefore => wasMatch(m(endingBefore))
    override def mismatched = wasMismatch
  }

  def matchIf(f: NonNegativeInt => Boolean, m: NonNegativeInt => NonNegativeInt = identity): ParseResult[NonNegativeInt, Result[NonNegativeInt]] = new {
    override def matched = endingBefore =>
      if(f(endingBefore)) wasMatch(m(endingBefore))
      else wasMismatch
    override def mismatched = wasMismatch
  }
}

opaque type Parse[Buffer] = (Buffer, NonNegativeInt) => Result[NonNegativeInt]

object Parse {

  implied PositionParser[Buffer] for Parser[Parse[Buffer]] {
    override def succeed: Parse[Buffer] = (buff, pos) => Result.wasMatch(pos)
    override def fail: Parse[Buffer] = (buff, pos) => Result.wasMismatch
  }

  implied PositionLookaheadParser[Buffer] for LookaheadParser[Parse[Buffer]] {
    override def positiveLookAhead(lhs: Parse[Buffer]): Parse[Buffer] = (buff, pos) =>
      lhs(buff, pos)(Result.handleMatch(_ => pos))

    override def negativeLookAhead(lhs: Parse[Buffer]): Parse[Buffer] = (buff, pos) => lhs(buff, pos)(new {
      override def matched = endingBefore => Result.wasMismatch
      override def mismatched = Result.wasMatch(pos)
    })
  }

  implied PositionParserLocations[Buffer, Token]
    given (TB: TokenBuffer[Buffer, Token]) for ParserLocations[Parse[Buffer]] {
    override def beginning = (buff, pos) =>
      if(pos == NonNegativeInt(0)) Result.wasMatch(pos)
      else Result.wasMismatch
    override def ending = (buff, pos) =>
      if(pos == buff.length) Result.wasMatch(pos)
      else Result.wasMismatch
  }

  implied PositionTokenParser[Buffer, Token]
    given (TB: TokenBuffer[Buffer, Token], E: Equiv[Token]) for TokenParser[Token, Parse[Buffer]] {
    override def anyToken: Parse[Buffer] = (buff, pos) =>
      Result.wasMatch(pos + 1)

      override def token(t: Token): Parse[Buffer] = (buff, pos) =>
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
    given (TB: TokenBuffer[Buffer, Token], E: Equiv[Token]) for BufferParser[Buffer, Parse[Buffer]] =
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


  implied PositionParseOneThenOther[Buffer] for ParseOneThenOther[Parse[Buffer], Parse[Buffer], Parse[Buffer]] =
    (lhs, rhs) => (buff, pos) =>
      lhs(buff, pos)(new {
        override def matched = rhs(buff, _)(new {
          override def matched = Result.wasMatch
          override def mismatched = Result.wasMismatch
        })
        override def mismatched = Result.wasMismatch
      })


  implied PositionParseOneOrOther[Buffer] for ParseOneOrOther[Parse[Buffer], Parse[Buffer], Parse[Buffer]] =
    (lhs, rhs) => (buff, pos) =>
      lhs(buff, pos)(new {
        override def matched = Result.wasMatch
        override def mismatched = rhs(buff, pos)(new {
          override def matched = Result.wasMatch
          override def mismatched = Result.wasMismatch
        })
      })


  implied RunPositionParser[Buffer] for RunDSL[Parse[Buffer], Buffer => Result[NonNegativeInt]] = p => p(_, NonNegativeInt(0))


  implied CapturePositionAsValue[Buffer, Token] given TokenBuffer[Buffer, Token] for ParserCapture[Buffer, Parse[Buffer], [A] => Value[Buffer, A]] {
    def (p: Parse[Buffer]) capture = Value { (buff, pos) =>
      p(buff, pos)(new {
        override def matched = end => ValueResult.wasMatch(end, buff.subBuffer(pos, end))

        override def mismatched = ValueResult.wasMismatch
      })
    }

  }

  inline def (p: Parse[Buffer]) apply[Buffer](buff: Buffer, pos: NonNegativeInt): Result[NonNegativeInt] = (p: (Buffer, NonNegativeInt) => Result[NonNegativeInt])(buff, pos)
}

implied [Buffer] for RunDSL[Parse[Buffer], Buffer => Result[NonNegativeInt]] = Parse.RunPositionParser


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