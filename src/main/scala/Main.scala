
@FunctionalInterface
trait Variable[B] {
  def variable(name: String): B
}


trait TruthValues[B] {
  def ⊤ : B
  def ⊥ : B
}

@FunctionalInterface
trait And[B] {
  def and(lhs: B, rhs: B): B
}

object And {
  instance AndAsSemigroup of AsSemigroup[And] {
    def (B: And[B]) asSemigroup[B]: Semigroup[B] = B.and
  }
}

@FunctionalInterface
trait Or[B] {
  def or(lhs: B, rhs: B): B
}

object Or {
  instance OrAsSemigroup of AsSemigroup[Or] {
    def (B: Or[B]) asSemigroup[B]: Semigroup[B] = B.or
  }
}

@FunctionalInterface
trait Not[B] {
  def not(lhs: B): B
}


object StringifyContext {

  opaque type Stringify = Appendable => Unit

  object Stringify {

    import Semigroup._

    def (s: String) a: Stringify = _ append s
    def apply(a: Appendable => Unit): Stringify = a

    instance StringifyTruthValues of TruthValues[Stringify] {
      override def ⊤ : Stringify = "⊤".a
      override def ⊥ : Stringify = "⊥".a
    }
    instance StringifyAnd of And[Stringify] {
      override def and(lhs: Stringify, rhs: Stringify): Stringify =
      "and(".a ++ lhs ++ ",".a ++ rhs ++ ")".a
    }
    instance StringifyOr of Or[Stringify] {
      override def or(lhs: Stringify, rhs: Stringify): Stringify =
        "or(".a ++ lhs ++ ",".a ++ rhs ++ ")".a
    }
    instance StringifyNot of Not[Stringify] {
      override def not(lhs: Stringify): Stringify =
      "not(".a ++ lhs ++ ")".a
    }
    instance StringifyVariable of Variable[Stringify] {
      override def variable(name: String): Stringify = "?".a ++ name.a
    }

    def run(s: Stringify, a: Appendable): Unit = s(a)
  }

  instance StringifySemigroup of Semigroup[Stringify] {
    override def append(lhs: Stringify, rhs: Stringify): Stringify =
      Stringify(a => { Stringify.run(lhs, a) ; Stringify.run(rhs, a) })
  }

  instance RunStringify of RunDSL[Stringify, Appendable => Unit] {
    override def runDSL(s: Stringify) = Stringify.run(s, _)
  }
}

import StringifyContext._

object BooleanContext {
  opaque type Bool = Boolean

  object Bool {
    instance BooleanTruthValues of TruthValues[Bool] {
      override def ⊤ : Bool = true
      override def ⊥ : Bool = false
    }
    instance BooleanAnd of And[Bool] {
      override def and(lhs: Bool, rhs: Bool): Bool= lhs && rhs
    }
    instance BooleanOr of Or[Bool] {
      override def or(lhs: Bool, rhs: Bool): Bool = lhs || rhs
    }
    instance BooleanNot of Not[Bool] {
      override def not(lhs: Bool): Bool = !lhs
    }
    private [BooleanContext] def run(b: Bool): Boolean = b
  }

  instance RunBool of RunDSL[Bool, Boolean] {
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
    instance def NNFNot[B]: Not[NNF[B]] = (lhs: NNF[B]) => new {
      override def negationOf: B = lhs.terminal
      override def terminal: B = lhs.negationOf
    }
    instance def NNFAnd[B] with (A: And[B], O: Or[B]): And[NNF[B]] = (lhs: NNF[B], rhs: NNF[B]) => new {
      override def terminal: B = A.and(lhs.terminal, rhs.terminal)
      override def negationOf: B = O.or(lhs.negationOf, rhs.negationOf)
    }
    instance def NNFOr[B] with (A: And[B], O: Or[B]): Or[NNF[B]] = (lhs: NNF[B], rhs: NNF[B]) => new {
      override def terminal: B = O.or(lhs.terminal, rhs.terminal)
      override def negationOf: B = A.and(lhs.negationOf, rhs.negationOf)
    }
    instance def NNFTruthValues[B] with (B: TruthValues[B]): TruthValues[NNF[B]] = new {
      def ⊤ : NNF[B] = new {
        override def terminal: B = B.⊤
        override def negationOf: B = B.⊥
      }
      def ⊥ : NNF[B] = new {
        override def terminal: B = B.⊥
        override def negationOf: B = B.⊤
      }
    }
    def NNFTerminal[A, B](f: A => B) with (N: Not[B]): A => NNF[B] = a => new {
      override def terminal: B = f(a)
      override def negationOf: B = N.not(f(a))
    }

    instance def NNFVariable[B] with (N: Not[B], V: Variable[B]): Variable[NNF[B]] = NNFTerminal(V.variable) apply

    private [NNFContext] def run[B](b: NNF[B]): B = b.terminal
  }

  instance def RunNNF[B]: RunDSL[NNF[B], B] = new {
    override def runDSL(b: NNF[B]): B = NNF.run(b)
  }
}

import NNFContext._

object VariableBinding {

  type MapBinding[B] = (String, TruthValues[B] |=> B)
  opaque type MapBindings[B] = List[MapBinding[B]]

  def (name: String) |-> [B](value: TruthValues[B] |=> B): MapBindings[B] = MapBindings.bind(name, value)

  object MapBindings {
    def empty[B]: MapBindings[B] = Nil
    def bind[B](binding: MapBinding[B]): MapBindings[B] = binding :: Nil

    instance def MapBindingsSemigroup[B]: Semigroup[MapBindings[B]] = _ ++ _

    def lookup[B](mb: MapBindings[B], name: String): Option[TruthValues[B] |=> B] = mb.find(_._1 == name).map(_._2)
  }
}

import VariableBinding._

object BindOrFailContext {
  opaque type BoundOrFail[+B] =
    Set[String] | // evaluate to the set of unbound variable names
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

    instance def BindVariable[B] with TruthValues[B]: Variable[BindOrFail[B]] = name => MapBindings.lookup(_, name) match {
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

    instance def BindAnd[B] with (A: And[B]): And[BindOrFail[B]] = BindSemigroup(A.asSemigroup).append
    instance def BindOr[B] with (O: Or[B]): Or[BindOrFail[B]] = BindSemigroup(O.asSemigroup).append

    instance def BindNot[B] with (N: Not[B]): Not[BindOrFail[B]] =
      lhs => b => lhs(b) fold (
        N.not(_).bound,
        identity(_).unbound
      )

    instance def BindTruthValue[B] with (T: TruthValues[B]): TruthValues[BindOrFail[B]] = new {
      override def ⊤ : BindOrFail[B] = _ => T.⊤.bound
      override def ⊥ : BindOrFail[B] = _ => T.⊥.bound

    }
  }

  instance def RunBindOrFail[B]: RunDSL[BindOrFail[B], MapBindings[B] => BoundOrFail[B]] = BindOrFail.unwrap
}

import BindOrFailContext._


object BindPartiallyContext {
  opaque type BindPartially[B] = MapBindings[B] => B

  object BindPartially {
    def unwrap[B](b: BindPartially[B]): MapBindings[B] => B = b

    instance def BindVariable[B] with (T: TruthValues[B], V: Variable[B]): Variable[BindPartially[B]] =
      name => MapBindings.lookup(_, name) match {
        case Some(b) => b
        case None => V.variable(name)
      }

    def BindSemigroup[B](S: Semigroup[B]): Semigroup[BindPartially[B]] =
      (lhs, rhs) => b => S.append(lhs(b), rhs(b))

    instance def BindAnd[B] with (A: And[B]): And[BindPartially[B]] = BindSemigroup(A.asSemigroup).append
    instance def BindOr [B] with (O: Or [B]): Or [BindPartially[B]] = BindSemigroup(O.asSemigroup).append
    instance def BindNot[B] with (N: Not[B]): Not[BindPartially[B]] = lhs => b => N.not(lhs(b))
  }

  implicit def RunBindPartially[B]: RunDSL[BindPartially[B], MapBindings[B] => B] = BindPartially.unwrap
}

import BindPartiallyContext._







object Main {

  import Semigroup._

  // syntax, because there isn't yet a nice way to deal with this without boilerplate - hint, hint
  inline def runDSL[Rep, Res](a: Rep) with (R: RunDSL[Rep, Res]): Res = R.runDSL(a)

  inline def ⊤ [B] with (B: TruthValues[B]): B = B.⊤
  inline def ⊥ [B] with (B: TruthValues[B]): B = B.⊥
  inline def and[B](lhs: B, rhs: B) with (B: And[B]): B = B.and(lhs, rhs)
  inline def or[B](lhs: B, rhs: B) with (B: Or[B]): B = B.or(lhs, rhs)
  inline def not[B](lhs: B) with (B: Not[B]): B = B.not(lhs)
  def (name: String) ? [B] with (B: Variable[B]): B = B.variable(name)

  // Some logical statements
  // These are here as defs rather than inline as expressions because type inference works for the instances within
  // defs but fails for expressions, due to the interaction of how implicits are prioritised in scope and how much
  // type information is used from the call context
  def andTF[B] with (A: And[B], T: TruthValues[B]): B = and(⊤, ⊥)
  def orTF[B] with (O: Or[B], T: TruthValues[B]): B = or(⊤, ⊥)
  def notF[B] with (N: Not[B], T: TruthValues[B]): B = not(⊥)
  def notAndTNotF[B] with (A: And[B], N: Not[B], T: TruthValues[B]): B = not(and(⊤, not(⊥)))

  def implication[B] with (A: And[B], N: Not[B], V: Variable[B]): B =
    not(and("a".?, not("b".?)))

  def main(args: Array[String]): Unit = {
    // this won't compile because of type inference being not good enough
//    runDSL[Rep = Stringify](and(⊤, ⊥))

    // these all will
    println("+ Some expressions")
    runDSL[Rep = Stringify](andTF) apply System.out ; println
    println(runDSL[Rep = Bool](andTF))
    println(runDSL[Rep = Bool](orTF))
    println(runDSL[Rep = Bool](notF))

    println("+ Raw and negation normal form")
    runDSL[Rep = Stringify](notAndTNotF)(System.out) ; println
//    println(runDSL[Rep = Bool](notAndTNotF))
    runDSL[Rep = Stringify](runDSL[Rep = NNF[Stringify]](notAndTNotF))(System.out) ; println

    println("+ Implication and negation normal form")
    runDSL[Rep = Stringify](implication)(System.out) ; println
    runDSL[Rep = Stringify](runDSL[Rep = NNF[Stringify]](implication))(System.out) ; println

    println("+ Falliable variable bindings")
    runDSL[Rep = BindOrFail[Stringify]](implication)(MapBindings.empty) fold (
      s => { runDSL[Rep = Stringify](s)(System.out) ; println },
    unbound => println(s"Unbound variables: $unbound")
    )

    runDSL[Rep = BindOrFail[Stringify]](implication)("a" |-> ⊤) fold (
      s => { runDSL[Rep = Stringify](s)(System.out) ; println },
    unbound => println(s"Unbound variables: $unbound")
    )

    runDSL[Rep = BindOrFail[Stringify]](implication)(("a" |-> ⊤[Stringify]) ++ ("b" |-> ⊥)) fold (
      s => { runDSL[Rep = Stringify](s)(System.out) ; println },
    unbound => println(s"Unbound variables: $unbound")
    )

    println("+ irrefutible variable binding")
    runDSL[Rep = Stringify](runDSL[Rep = BindPartially[Stringify]](implication)("a" |-> ⊤))(System.out) ; println
  }
}
