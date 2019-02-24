import scala.language.implicitConversions

object Main {

  def main(args: Array[String]): Unit = { logic ; parser }

  def logic: Unit = {

    inline def ⊤ [B] given (B: TruthValues[B]): B = B.⊤
    inline def ⊥ [B] given (B: TruthValues[B]): B = B.⊥
    inline def and[B](lhs: B, rhs: B) given (B: And[B]): B = B.and(lhs, rhs)
    inline def or[B](lhs: B, rhs: B) given (B: Or[B]): B = B.or(lhs, rhs)
    inline def not[B](lhs: B) given (B: Not[B]): B = B.not(lhs)
    def (name: String) ? [B] given (B: Variable[B]): B = B.variable(name)

    // Some logical statements
    def andTF[B] given (A: And[B], T: TruthValues[B]): B = and(⊤, ⊥)
    def orTF[B] given (O: Or[B], T: TruthValues[B]): B = or(⊤, ⊥)
    def notF[B] given (N: Not[B], T: TruthValues[B]): B = not(⊥)
    def notAndTNotF[B] given (A: And[B], N: Not[B], T: TruthValues[B]): B = not(and(⊤, not(⊥)))

    def implication[B] given (A: And[B], N: Not[B], V: Variable[B]): B =
    not(and("a".?, not("b".?)))

    println("+ Some expressions")
    (andTF : Stringify).run(System.out) ; println

    println((andTF : Bool).run)
    println((orTF : Bool).run)
    println((notF : Bool).run)

    println("+ Raw and negation normal form")
    (notAndTNotF : Stringify).run(System.out) ; println
    (notAndTNotF : NNF[Stringify]).run.run(System.out) ; println

    println("+ Implication and negation normal form")
    (implication : Stringify).run(System.out) ; println
    (implication : NNF[Stringify]).run.run(System.out) ; println

    println("+ Variable bindings with fail on non-bound")
    (implication : BindOrFail[Stringify]).run(MapBindings.empty)(new {
      override def bound(s: Stringify) = { s.run(System.out) ; println }
      override def unbound(unbound: Set[String]) = println(s"Unbound variables: $unbound")
    })

    (implication : BindOrFail[Stringify]).run("a" |-> ⊤)(new {
      override def bound(s: Stringify) = { s.run(System.out) ; println }
      override def unbound(unbound: Set[String]) = println(s"Unbound variables: $unbound")
    })

    (implication : BindOrFail[Stringify]).run(("a" |-> ⊤[Stringify]) ++ ("b" |-> ⊥))(new {
      override def bound(s: Stringify) = { s.run(System.out) ; println }
      override def unbound(unbound: Set[String]) = println(s"Unbound variables: $unbound")
    })

    println("+ Variable binding with no change on non-bound")
    (implication : BindIfPossible[Stringify]).run("a" |-> ⊤).run(System.out) ; println
    (implication : NNF[BindIfPossible[Stringify]]).run.run("a" |-> ⊤).run(System.out) ; println
  }

  def parser: Unit = {
    import implied Position._
    import Value._

    val xy = 'x' ~ 'y'
    val xyEnd = xy ~ ParserSyntax.ending
    val xHiMum = 'x' ~ "Hi mum"
    val hiMumX = "hi mum" ~ 'x'
    val hiMumHiMum = "hi mum" ~ "hi mum"

    val xyHiMum = xy ~ "Hi mum"
    val hiMumXxy = hiMumX ~ xy

    val printPosition:  MatchPosition[Unit] = new {
      override def matched(endingBefore: NonNegativeInt) = println(s"Matched input up until $endingBefore")
      override def mismatched: Unit = println("Mismatched input")
    }

    def printValue[A]:  MatchValue[Const[Unit], A] = new {
      override def matched(endingBefore: NonNegativeInt, value: A) = println(s"Matched input up until $endingBefore with $value")
      override def mismatched = println("Mismatched input")
    }

    println("Parsing with xy")
    xy.run("xy")(printPosition)
    xy.run("x")(printPosition)
    xy.run("xyz")(printPosition)
    xy.run("yx")(printPosition)

    println("Parsing with xy~ending")
    xyEnd.run("xy")(printPosition)
    xyEnd.run("x")(printPosition)
    xyEnd.run("xyz")(printPosition)
    xyEnd.run("yx")(printPosition)

    println("Capturing with xy")
    println("Parsing with xy")
    xy.capture.run("xy")(printValue)
    xy.capture.run("x")(printValue)
    xy.capture.run("xyz")(printValue)
    xy.capture.run("yx")(printValue)

    println("Composing capture")
    val cXy = xy.capture ~ hiMumX
    cXy.run("xyhi mumx")(printValue)
    cXy.run("xyhi mum")(printValue)

    val cHiMumx = xy ~ hiMumX.capture
    cHiMumx.run("xyhi mumx")(printValue)
    cHiMumx.run("xyhi mum")(printValue)

    val cXyHiMumx = xy.capture ~ hiMumX.capture
    cXyHiMumx.run("xyhi mumx")(printValue)
    cXyHiMumx.run("xyhi mum")(printValue)

  }
}
