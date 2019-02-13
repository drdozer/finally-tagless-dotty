
object Main {

  import BoundOrFail.fold
  import Semigroup._

  inline def ⊤ [B] given (B: TruthValues[B]): B = B.⊤
  inline def ⊥ [B] given (B: TruthValues[B]): B = B.⊥
  inline def and[B](lhs: B, rhs: B) given (B: And[B]): B = B.and(lhs, rhs)
  inline def or[B](lhs: B, rhs: B) given (B: Or[B]): B = B.or(lhs, rhs)
  inline def not[B](lhs: B) given (B: Not[B]): B = B.not(lhs)
  def (name: String) ? [B] given (B: Variable[B]): B = B.variable(name)

  // Some logical statements
  // These are here as defs rather than inline as expressions because type inference works for the implieds givenin
  // defs but fails for expressions, due to the interaction for how implicits are prioritised in scope and how much
  // type information is used from the call context
  def andTF[B] given (A: And[B], T: TruthValues[B]): B = and(⊤, ⊥)
  def orTF[B] given (O: Or[B], T: TruthValues[B]): B = or(⊤, ⊥)
  def notF[B] given (N: Not[B], T: TruthValues[B]): B = not(⊥)
  def notAndTNotF[B] given (A: And[B], N: Not[B], T: TruthValues[B]): B = not(and(⊤, not(⊥)))

  def implication[B] given (A: And[B], N: Not[B], V: Variable[B]): B =
  not(and("a".?, not("b".?)))

  def main(args: Array[String]): Unit = {
    import RunDSL.run

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

    println("+ Falliable variable bindings")
    (implication : BindOrFail[Stringify]).run(MapBindings.empty) fold (
      s => { s.run(System.out) ; println },
      unbound => println(s"Unbound variables: $unbound")
    )

    (implication : BindOrFail[Stringify]).run("a" |-> ⊤) fold (
      s => { s.run(System.out) ; println },
      unbound => println(s"Unbound variables: $unbound")
    )

    (implication : BindOrFail[Stringify]).run(("a" |-> ⊤[Stringify]) ++ ("b" |-> ⊥)) fold (
      s => { s.run(System.out) ; println },
      unbound => println(s"Unbound variables: $unbound")
    )

    println("+ irrefutible variable binding")
    (implication : BindPartially[Stringify]).run("a" |-> ⊤).run(System.out) ; println
    (implication : NNF[BindPartially[Stringify]]).run.run("a" |-> ⊤).run(System.out) ; println


  }
}
