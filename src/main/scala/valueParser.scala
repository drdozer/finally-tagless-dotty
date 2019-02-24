import scala.language.{higherKinds, implicitConversions}

trait MatchValue[V[_]] {
  def matched[A](endingBefore: NonNegativeInt, value: A): V[A]
  def mismatched[A]: V[A]
}

trait ValueResult[A] {
  def apply[V[_]](mv: MatchValue[V]): V[A]
}

object ValueResult {
  def wasMatch[A](endingBefore: NonNegativeInt, value: A): ValueResult[A] = new {
    def apply[V[_]](mv: MatchValue[V]) = mv.matched(endingBefore, value)
  }
  def wasMismatch[A]: ValueResult[A] = new {
    def apply[V[_]](mv: MatchValue[V]) = mv.mismatched
  }
}

opaque type Value[Buffer, A] = (Buffer, NonNegativeInt) => ValueResult[A]

object Value {
  import Position._

  def capture[Buffer, Token](p: Position[Buffer]) given TokenBuffer[Buffer, Token]: Value[Buffer, Buffer] =
    (buff, pos) =>
      p(buff, pos)(new {
        override def matched(end: NonNegativeInt) = ValueResult.wasMatch(end, buff.subBuffer(pos, end))
        override def mismatched = ValueResult.wasMismatch
      })

  implied RunValue[Buffer, A] for RunDSL[Value[Buffer, A], Buffer => ValueResult[A]] = p => p(_, NonNegativeInt(0))
}

implied [Buffer, A] for RunDSL[Value[Buffer, A], Buffer => ValueResult[A]] = Value.RunValue

//implied ValueSyntax {
//  def (p: Position[Buffer]) capture[Buffer, Token] given TokenBuffer[Buffer, Token]: Value[Buffer, Buffer] =
//    Value.capture(p)
//}

