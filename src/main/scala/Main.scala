
import StringifyContext._


object NNFContext {
  trait NNF[T] {
    def negationOf: T
    def terminal: T
  }

  object NNF {
    implied NNFNot[B] for Not[NNF[B]] = (lhs: NNF[B]) => new {
      override def negationOf: B = lhs.terminal
      override def terminal: B = lhs.negationOf
    }
    implied NNFAnd[B] given (A: And[B], O: Or[B]) for And[NNF[B]] = (lhs: NNF[B], rhs: NNF[B]) => new {
      override def terminal: B = A.and(lhs.terminal, rhs.terminal)
      override def negationOf: B = O.or(lhs.negationOf, rhs.negationOf)
    }
    implied NNFOr[B] given (A: And[B], O: Or[B]) for Or[NNF[B]] = (lhs: NNF[B], rhs: NNF[B]) => new {
      override def terminal: B = O.or(lhs.terminal, rhs.terminal)
      override def negationOf: B = A.and(lhs.negationOf, rhs.negationOf)
    }
    implied NNFTruthValues[B] given (B: TruthValues[B]) for TruthValues[NNF[B]] = new {
      def ⊤ : NNF[B] = new {
        override def terminal: B = B.⊤
        override def negationOf: B = B.⊥
      }
      def ⊥ : NNF[B] = new {
        override def terminal: B = B.⊥
        override def negationOf: B = B.⊤
      }
    }
    def NNFTerminal[A, B](f: A => B) given (N: Not[B]): A => NNF[B] = a => new {
      override def terminal: B = f(a)
      override def negationOf: B = N.not(f(a))
    }

    implied NNFVariable[B] given (N: Not[B], V: Variable[B]) for Variable[NNF[B]] = NNFTerminal(V.variable) apply

    private [NNFContext] def run[B](b: NNF[B]): B = b.terminal
  }

  implied RunNNF[B] for RunDSL[NNF[B], B] {
    override def runDSL(b: NNF[B]): B = NNF.run(b)
  }
}

import NNFContext._
import BoundOrFail.fold







object Main {

  import Semigroup._

  // syntax, because there isn't yet a nice way to deal given this givenout boilerplate - hint, hint
  inline def runDSL[Rep, Res](a: Rep) given (R: RunDSL[Rep, Res]): Res = R.runDSL(a)

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
    // this won't compile because for type inference being not good enough
//    runDSL[Rep = Stringify](and(⊤, ⊥))
    import RunDSL.run

    // these all will
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
