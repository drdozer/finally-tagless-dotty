
@FunctionalInterface
trait Variable[B] {
  def variable(name: String): B
}


trait TruthValues[B] {
  def ⊤ : B
  def ⊥ : B
  inline def of(b: Boolean): B = if(b) ⊤ else ⊥
}



trait Strandedness[S] {
  def topStrand: S
  def bottomStrand: S
}

@FunctionalInterface
trait SameStrand[S, B] {
  def sameStrand(lhs: S, rhs: S): B
}



@FunctionalInterface
trait DifferentStrand[S, B] {
  def differentStrand(lhs: S, rhs: S): S
}

@FunctionalInterface
trait And[B] {
  def and(lhs: B, rhs: B): B
}

object And {
  implied AndAsSemigroup for AsSemigroup[And] {
    def (B: And[B]) asSemigroup[B]: Semigroup[B] = B.and
  }
}

@FunctionalInterface
trait Or[B] {
  def or(lhs: B, rhs: B): B
}

object Or {
  implied OrAsSemigroup for AsSemigroup[Or] {
    def (B: Or[B]) asSemigroup[B]: Semigroup[B] = B.or
  }
}

@FunctionalInterface
trait Not[B] {
  def not(lhs: B): B
}

@FunctionalInterface
trait Lt[P, B] {
  def lt(lhs: P, rhs: P): B
}

object Lt {
  implied IntLt[B] given (t: TruthValues[B]) for Lt[Int, B] = (lhs, rhs) => t.of(lhs < rhs)
  implied NNFLt[P, B] given (t: Lt[P, B], n: Gteq[P, B]) for Lt[P, NNFContext.NNF[B]] = (lhs, rhs) => new {
    override def terminal: B = t.lt(lhs, rhs)
    override def negationOf: B = n.gteq(lhs, rhs)
  }
}

@FunctionalInterface
trait Lteq[P, B] {
  def lteq(lhs: P, rhs: P): B
}

object Lteq {
  implied IntLteq[B] given (t: TruthValues[B]) for Lteq[Int, B] = (lhs, rhs) => t.of(lhs <= rhs)
  implied NNFLteq[P, B] given (t: Lteq[P, B], n: Gt[P, B]) for Lteq[P, NNFContext.NNF[B]] = (lhs, rhs) => new {
    override def terminal: B = t.lteq(lhs, rhs)
    override def negationOf: B = n.gt(lhs, rhs)
  }
}

@FunctionalInterface
trait Gteq[P, B] {
  def gteq(lhs: P, rhs: P): B
}

object Gteq {
  implied IntGteq[B] given (t: TruthValues[B]) for Gteq[Int, B] = (lhs, rhs) => t.of(lhs >= rhs)
  implied NNFGteq[P, B] given (t: Gteq[P, B], n: Lt[P, B]) for Gteq[P, NNFContext.NNF[B]] = (lhs, rhs) => new {
    override def terminal: B = t.gteq(lhs, rhs)
    override def negationOf: B = n.lt(lhs, rhs)
  }
}

@FunctionalInterface
trait Gt[P, B] {
  def gt(lhs: P, rhs: P): B
}

object Gt {
  implied IntGt[B] given (t: TruthValues[B]) for Gt[Int, B] = (lhs, rhs) => t.of(lhs > rhs)
  implied NNFGt[P, B] given (t: Gt[P, B], n: Lteq[P, B]) for Gt[P, NNFContext.NNF[B]] = (lhs, rhs) => new {
    override def terminal: B = t.gt(lhs, rhs)
    override def negationOf: B = n.lteq(lhs, rhs)
  }
}

@FunctionalInterface
trait Eq[P, B] {
  def eq(lhs: P, rhs: P): B
}

object Eq {
  implied IntEq[B] given (t: TruthValues[B]) for Eq[Int, B] = (lhs, rhs) => t.of(lhs == rhs)
  implied NNFEq[P, B] given (t: Eq[P, B], n: Neq[P, B]) for Eq[P, NNFContext.NNF[B]] = (lhs, rhs) => new {
    override def terminal: B = t.eq(lhs, rhs)
    override def negationOf: B = n.neq(lhs, rhs)
  }
}

@FunctionalInterface
trait Neq[P, B] {
  def neq(lhs: P, rhs: P): B
}

object Neq {
  implied IntNeq[B] given (t: TruthValues[B]) for Neq[Int, B] = (lhs, rhs) => t.of(lhs != rhs)
  implied NNFNeq[P, B] given (t: Neq[P, B], n: Eq[P, B]) for Neq[P, NNFContext.NNF[B]] = (lhs, rhs) => new {
    override def terminal: B = t.neq(lhs, rhs)
    override def negationOf: B = n.eq(lhs, rhs)
  }
}


object TotalOrderContext {
  opaque type TotalOrder[P] = P

  object TotalOrder {
    implied LtTotalOrder[P, B] given (pb: Lt[P, B]) for Lt[TotalOrder[P], B] = (lhs, rhs) => pb.lt(lhs, rhs)
    implied LteqTotalOrder[P, B] given (l: Lt[P, B], e: Eq[P, B], o: Or[B]) for Lteq[TotalOrder[P], B] = (lhs, rhs) =>
      o.or(
        l.lt(lhs, rhs),
        e.eq(lhs, rhs)
      )
    implied GteqTotalOrder[P, B] given (l: Lt[P, B], e: Eq[P, B], o: Or[B]) for Gteq[TotalOrder[P], B] = (lhs, rhs) =>
      o.or(
        l.lt(rhs, lhs),
        e.eq(rhs, lhs)
      )
    implied GtOrder[P, B] given (pb: Lt[P, B]) for Gt[TotalOrder[P], B] = (lhs, rhs) => pb.lt(rhs, lhs)
    implied EqTotalOrder[P, B] given (e: Eq[P, B]) for Eq[TotalOrder[P], B] = (lhs, rhs) => e.eq(lhs, rhs)
    implied NeqTotalOrder[P, B] given (l: Lt[P, B], o: Or[B]) for Neq[TotalOrder[P], B] = (lhs, rhs) =>
      o.or(
        l.lt(lhs, rhs),
        l.lt(rhs, lhs)
      )
  }

}

object StringifyContext {

  opaque type Stringify = Appendable => Unit

  object Stringify {

    import Semigroup._

    def (s: String) a: Stringify = _ append s
    def apply(a: Appendable => Unit): Stringify = a

    implied StringifyTruthValues for TruthValues[Stringify] {
      override def ⊤ : Stringify = "⊤".a
      override def ⊥ : Stringify = "⊥".a
    }
    implied StringifyAnd for And[Stringify] {
      override def and(lhs: Stringify, rhs: Stringify): Stringify =
      "and(".a ++ lhs ++ ",".a ++ rhs ++ ")".a
    }
    implied StringifyOr for Or[Stringify] {
      override def or(lhs: Stringify, rhs: Stringify): Stringify =
        "or(".a ++ lhs ++ ",".a ++ rhs ++ ")".a
    }
    implied StringifyNot for Not[Stringify] {
      override def not(lhs: Stringify): Stringify =
      "not(".a ++ lhs ++ ")".a
    }
    implied StringifyVariable for Variable[Stringify] {
      override def variable(name: String): Stringify = "?".a ++ name.a
    }

    def run(s: Stringify, a: Appendable): Unit = s(a)
  }

  implied StringifySemigroup for Semigroup[Stringify] {
    override def append(lhs: Stringify, rhs: Stringify): Stringify =
      Stringify(a => { Stringify.run(lhs, a) ; Stringify.run(rhs, a) })
  }

  implied RunStringify for RunDSL[Stringify, Appendable => Unit] {
    override def runDSL(s: Stringify) = Stringify.run(s, _)
  }
}

import StringifyContext._

object BooleanContext {
  opaque type Bool = Boolean

  object Bool {
    implied BooleanTruthValues for TruthValues[Bool] {
      override def ⊤ : Bool = true
      override def ⊥ : Bool = false
    }
    implied BooleanAnd for And[Bool] {
      override def and(lhs: Bool, rhs: Bool): Bool= lhs && rhs
    }
    implied BooleanOr for Or[Bool] {
      override def or(lhs: Bool, rhs: Bool): Bool = lhs || rhs
    }
    implied BooleanNot for Not[Bool] {
      override def not(lhs: Bool): Bool = !lhs
    }
    private [BooleanContext] def run(b: Bool): Boolean = b
  }

  implied RunBool for RunDSL[Bool, Boolean] {
    def runDSL(b: Bool): Boolean = Bool.run(b)
  }
}

import BooleanContext._

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

object VariableBinding {

  type MapBinding[B] = (String, given TruthValues[B] => B)
  opaque type MapBindings[B] = List[MapBinding[B]]

  def (name: String) |-> [B](value: given TruthValues[B] => B): MapBindings[B] = MapBindings.bind(name, value)

  object MapBindings {
    def empty[B]: MapBindings[B] = Nil
    def bind[B](binding: MapBinding[B]): MapBindings[B] = binding :: Nil

    implied MapBindingsSemigroup[B] for Semigroup[MapBindings[B]] = _ ++ _

    def lookup[B](mb: MapBindings[B], name: String): Option[given TruthValues[B] => B] = mb.find(_._1 == name).map(_._2)
  }
}

import VariableBinding._

object BindOrFailContext {
  opaque type BoundOrFail[+B] =
    Set[String] | // evaluate to the set for unbound variable names
      B // or a value if all are bound

  object BoundOrFail {
    def (s: Set[String]) unbound[B]: BoundOrFail[Nothing] = s
    def (b: B) bound[B]: BoundOrFail[B] = b
  }

  import BoundOrFail._
  def (b: BoundOrFail[B]) fold[B, C](onSuccess: B => C, onFailure: Set[String] => C): C = b match {
    case f: Set[String] => onFailure(f)
    case s: B => onSuccess(s)
  }


  opaque type BindOrFail[B] = MapBindings[B] => BoundOrFail[B]

  object BindOrFail {
    def unwrap[B](b: BindOrFail[B]): MapBindings[B] => BoundOrFail[B] = b

    implied BindVariable[B] given TruthValues[B] for Variable[BindOrFail[B]] = name => MapBindings.lookup(_, name) match {
      case Some(b) => b.bound
      case None => Set(name).unbound
    }


    def BindSemigroup[B](S: Semigroup[B]): Semigroup[BindOrFail[B]] =
      (lhs, rhs) => b => (lhs(b), rhs(b)) match {
        case (lb: Set[String], rb: Set[String]) => (lb ++ rb).unbound
        case (lb: Set[String], _: B) => lb.unbound
        case (_: B, rb: Set[String]) => rb.unbound
        case (lb: B, rb: B) => S.append(lb, rb).bound
      }

    implied BindAnd[B] given (A: And[B]) for And[BindOrFail[B]] = BindSemigroup(A.asSemigroup).append
    implied BindOr[B] given (O: Or[B]) for Or[BindOrFail[B]] = BindSemigroup(O.asSemigroup).append

    implied BindNot[B] given (N: Not[B]) for Not[BindOrFail[B]] =
      lhs => b => lhs(b) fold (
        N.not(_).bound,
        identity(_).unbound
      )

    implied BindTruthValue[B] given (T: TruthValues[B]) for TruthValues[BindOrFail[B]] = new {
      override def ⊤ : BindOrFail[B] = _ => T.⊤.bound
      override def ⊥ : BindOrFail[B] = _ => T.⊥.bound

    }
  }

  implied RunBindOrFail[B] for RunDSL[BindOrFail[B], MapBindings[B] => BoundOrFail[B]] = BindOrFail.unwrap
}

import BindOrFailContext._


object BindPartiallyContext {
  opaque type BindPartially[B] = MapBindings[B] => B

  object BindPartially {
    def unwrap[B](b: BindPartially[B]): MapBindings[B] => B = b

    implied BindVariable[B] given (T: TruthValues[B], V: Variable[B]) for Variable[BindPartially[B]] =
      name => MapBindings.lookup(_, name) match {
        case Some(b) => b
        case None => V.variable(name)
      }

    def BindSemigroup[B](S: Semigroup[B]): Semigroup[BindPartially[B]] =
      (lhs, rhs) => b => S.append(lhs(b), rhs(b))

    implied BindAnd[B] given (A: And[B]) for And[BindPartially[B]] = BindSemigroup(A.asSemigroup).append
    implied BindOr [B] given (O: Or [B]) for Or [BindPartially[B]] = BindSemigroup(O.asSemigroup).append
    implied BindNot[B] given (N: Not[B]) for Not[BindPartially[B]] = lhs => b => N.not(lhs(b))
  }

  implied RunBindPartially[B] for RunDSL[BindPartially[B], MapBindings[B] => B] = BindPartially.unwrap
}

import BindPartiallyContext._







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
