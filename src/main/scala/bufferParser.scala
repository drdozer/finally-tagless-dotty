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
  def wasMatch[S](s: S): Result[S] = new {
    override def apply[M](mr: ParseResult[S, M]): M = mr.matched(s)
  }
  // object? val? def? what will be best for inlining/erasing?
  def wasMismatch[S]: Result[S] = new {
    override def apply[M](mr: ParseResult[S, M]): M = mr.mismatched
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

// todo: there's a dotty bug with type aliases for opaques
// todo: it works if we remove the `opaque` modifier from `Parse`
type Parse[Buffer, In, Out] = (Buffer, In) => Result[Out]


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