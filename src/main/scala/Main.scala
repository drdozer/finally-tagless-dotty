@FunctionalInterface
trait Semigroup[S] {
  def append(lhs: S, rhs: S): S
}

object Semigroup {
  def (lhs: S) ++ [S](rhs: S)(implicit S: Semigroup[S]): S = S.append(lhs, rhs)
}

@FunctionalInterface
trait AsSemigroup[S[_]] {
  def (s: S[T]) asSemigroup[T]: Semigroup[T]
}

import Semigroup._

@FunctionalInterface
trait RunDSL[Rep, Res] {
  def runDSL(a: Rep): Res
}

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
  implicit object AndAsSemigroup extends AsSemigroup[And] {
    def (B: And[B]) asSemigroup[B]: Semigroup[B] = B.and
  }
}

@FunctionalInterface
trait Or[B] {
  def or(lhs: B, rhs: B): B
}

object Or {
  implicit object OrAsSemigroup extends AsSemigroup[Or] {
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
    def (s: String) a: Stringify = _ append s
    def apply(a: Appendable => Unit): Stringify = a

    implicit object StringifyTruthValues extends TruthValues[Stringify] {
      override def ⊤ : Stringify = "⊤".a
      override def ⊥ : Stringify = "⊥".a
    }
    implicit object StringifyAnd extends And[Stringify] {
      override def and(lhs: Stringify, rhs: Stringify): Stringify =
      "and(".a ++ lhs ++ ",".a ++ rhs ++ ")".a
    }
    implicit object StringifyOr extends Or[Stringify] {
      override def or(lhs: Stringify, rhs: Stringify): Stringify =
        "or(".a ++ lhs ++ ",".a ++ rhs ++ ")".a
    }
    implicit object StringifyNot extends Not[Stringify] {
      override def not(lhs: Stringify): Stringify =
      "not(".a ++ lhs ++ ")".a
    }
    implicit object StringifyVariable extends Variable[Stringify] {
      override def variable(name: String): Stringify = "?".a ++ name.a
    }

    def run(s: Stringify, a: Appendable): Unit = s(a)
  }

  implicit object StringifySemigroup extends Semigroup[Stringify] {
    override def append(lhs: Stringify, rhs: Stringify): Stringify =
      Stringify(a => { Stringify.run(lhs, a) ; Stringify.run(rhs, a) })
  }

  implicit object RunStringify extends RunDSL[Stringify, Appendable => Unit] {
    override def runDSL(s: Stringify) = Stringify.run(s, _)
  }
}

import StringifyContext._

object BooleanContext {
  opaque type Bool = Boolean

  object Bool {
    implicit object BooleanTruthValues extends TruthValues[Bool] {
      override def ⊤ : Bool = true
      override def ⊥ : Bool = false
    }
    implicit object BooleanAnd extends And[Bool] {
      override def and(lhs: Bool, rhs: Bool): Bool= lhs && rhs
    }
    implicit object BooleanOr extends Or[Bool] {
      override def or(lhs: Bool, rhs: Bool): Bool = lhs || rhs
    }
    implicit object BooleanNot extends Not[Bool] {
      override def not(lhs: Bool): Bool = !lhs
    }
    private [BooleanContext] def run(b: Bool): Boolean = b
  }

  implicit object RunBool extends RunDSL[Bool, Boolean] {
    def runDSL(b: Bool): Boolean = Bool.run(b)
  }
}

import BooleanContext._

object NNFContext {
  opaque type NNF[T] = Boolean => T

  object NNF {
    implicit def NNFNot[B]: Not[NNF[B]] = (lhs: NNF[B]) => ctx => lhs(!ctx)
    implicit def NNFAnd[B](implicit A: And[B], O: Or[B]): And[NNF[B]] = (lhs: NNF[B], rhs: NNF[B]) => {
      case true  => A.and(lhs(true), rhs(true))
      case false => O.or(lhs(false), rhs(false))
    }
    implicit def NNFOr[B](implicit A: And[B], O: Or[B]): Or[NNF[B]] = (lhs: NNF[B], rhs: NNF[B]) => {
      case true  => O.or(lhs(true), rhs(true))
      case false => A.and(lhs(false), rhs(false))
    }
    implicit def NNFTruthValues[B](implicit B: TruthValues[B]): TruthValues[NNF[B]] = new {
      def ⊤ : NNF[B] = {
        case true  => B.⊤
        case false => B.⊥
      }
      def ⊥ : NNF[B] = {
        case true  => B.⊥
        case false => B.⊤
      }
    }
    def NNFTerminal[A, B](f: A => B)(implicit N: Not[B]): A => NNF[B] = a => {
      case true => f(a)
      case false => N.not(f(a))
    }

    implicit def NNFVariable[B](implicit N: Not[B], V: Variable[B]): Variable[NNF[B]] = NNFTerminal(V.variable) apply

    private [NNFContext] def run[B](b: NNF[B]): B = b(true)
  }

  implicit def RunNNF[B]: RunDSL[NNF[B], B] = new {
    override def runDSL(b: NNF[B]): B = NNF.run(b)
  }
}

import NNFContext._

object VariableBinding {
  def (name: String) |-> [B](value: B): MapBinding[B] = MapBinding.bind(name, value)

  opaque type MapBinding[B] = Map[String, B]

  object MapBinding {
    def empty[B]: MapBinding[B] = Map.empty
    def bind[B](name: String, value: B): MapBinding[B] = Map((name, value))

    implicit def MapBindingSemigroup[B]: Semigroup[MapBinding[B]] = (_: Map[String, B]) ++ _

    def lookup[B](mb: MapBinding[B], name: String): Option[B] = mb.get(name)
  }
}

import VariableBinding._

object BindOrFailContext {
  opaque type BindOrFail[B] = MapBinding[B] =>
    Set[String] | // evaluate to the set of unbound variable names
    B // or a value if all are bound


  object BindOrFail {
    def unwrap[B](b: BindOrFail[B]): MapBinding[B] => Set[String] | B = b

    implicit def BindVariable[B]: Variable[BindOrFail[B]] = name => MapBinding.lookup(_, name) match {
      case Some(b) => b
      case None => Set(name)
    }


    def BindSemigroup[B](S: Semigroup[B]): Semigroup[BindOrFail[B]] =
      (lhs, rhs) => b => (lhs(b), rhs(b)) match {
        case (lb: Set[String], rb: Set[String]) => lb ++ rb
        case (lb: Set[String], _: B) => lb
        case (_: B, rb: Set[String]) => rb
        case (lb: B, rb: B) => S.append(lb, rb)
      }

    implicit def BindAnd[B](implicit A: And[B]): And[BindOrFail[B]] = BindSemigroup(A.asSemigroup).append
    implicit def BindOr[B](implicit O: Or[B]): Or[BindOrFail[B]] = BindSemigroup(O.asSemigroup).append

    implicit def BindNot[B](implicit N: Not[B]): Not[BindOrFail[B]] =
      lhs => b => lhs(b) match {
        case (lb: Set[String]) => lb
        case (lb: B) => N.not(lb)
      }

    implicit def BindTruthValue[B](implicit T: TruthValues[B]): TruthValues[BindOrFail[B]] = new {
      override def ⊤ : BindOrFail[B] = _ => T.⊤
      override def ⊥ : BindOrFail[B] = _ => T.⊥

    }
  }

  implicit def RunBindOrFail[B]: RunDSL[BindOrFail[B], MapBinding[B] => (Set[String] | B)] = BindOrFail.unwrap
}

import BindOrFailContext._


object BindPartiallyContext {
  opaque type BindPartially[B] = MapBinding[B] => B

  object BindPartially {
    def unwrap[B](b: BindPartially[B]): MapBinding[B] => B = b

    implicit def BindVariable[B](implicit V: Variable[B]): Variable[BindPartially[B]] =
      name => MapBinding.lookup(_, name) match {
        case Some(b) => b
        case None => V.variable(name)
      }

    def BindSemigroup[B](S: Semigroup[B]): Semigroup[BindPartially[B]] =
      (lhs, rhs) => b => S.append(lhs(b), rhs(b))

    implicit def BindAnd[B](implicit A: And[B]): And[BindPartially[B]] = BindSemigroup(A.asSemigroup).append
    implicit def BindOr[B](implicit O: Or[B]): Or[BindPartially[B]] = BindSemigroup(O.asSemigroup).append
    implicit def BindNot[B](implicit N: Not[B]): Not[BindPartially[B]] = lhs => b => N.not(lhs(b))
  }
}

object Main {
  // syntax, because there isn't yet a nice way to deal with this without boilerplate - hint, hint
  inline def runDSL[Rep, Res](a: Rep)(implicit R: RunDSL[Rep, Res]): Res = R.runDSL(a)

  inline def ⊤ [B](implicit B: TruthValues[B]): B = B.⊤
  inline def ⊥ [B](implicit B: TruthValues[B]): B = B.⊥
  inline def and[B](lhs: B, rhs: B)(implicit B: And[B]): B = B.and(lhs, rhs)
  inline def or[B](lhs: B, rhs: B)(implicit B: Or[B]): B = B.or(lhs, rhs)
  inline def not[B](lhs: B)(implicit B: Not[B]): B = B.not(lhs)
  def (name: String) ? [B](implicit B: Variable[B]): B = B.variable(name)

  // Some logical statements
  // These are here as defs rather than inline as expressions because type inference works for the instances within
  // defs but fails for expressions, due to the interaction of how implicits are prioritised in scope and how much
  // type information is used from the call context
  def andTF[B](implicit A: And[B], T: TruthValues[B]): B = and(⊤, ⊥)
  def orTF[B](implicit O: Or[B], T: TruthValues[B]): B = or(⊤, ⊥)
  def notF[B](implicit N: Not[B], T: TruthValues[B]): B = not(⊥)
  def notAndTNotF[B](implicit A: And[B], N: Not[B], T: TruthValues[B]): B = not(and(⊤, not(⊥)))

  def implication[B](implicit A: And[B], N: Not[B], V: Variable[B]): B =
    not(and("a".?, not("b".?)))

  def main(args: Array[String]): Unit = {
    // this won't compile because of type inference being not good enough
//    runDSL[Rep = Stringify](and(⊤, ⊥))

    // these all will
    runDSL[Rep = Stringify](andTF) apply System.out ; println
    println(runDSL[Rep = Bool](andTF))
    println(runDSL[Rep = Bool](orTF))
    println(runDSL[Rep = Bool](notF))

    runDSL[Rep = Stringify](notAndTNotF) apply System.out ; println
    println(runDSL[Rep = Bool](notAndTNotF))
    runDSL[Rep = Stringify](runDSL[Rep = NNF[Stringify]](notAndTNotF)) apply System.out ; println

    runDSL[Rep = Stringify](implication) apply System.out ; println
    runDSL[Rep = Stringify](runDSL[Rep = NNF[Stringify]](implication)) apply System.out ; println

    runDSL[Rep = BindOrFail[Stringify]](implication) apply MapBinding.empty match {
      case (unbound: Set[String]) =>
        println(s"Unbound variables: $unbound")
      case (s: Stringify) =>
        runDSL[Rep = Stringify](s) apply System.out
        println
    }

    def bindA[B](implicit T: TruthValues[B]) = "a" |-> ⊤

    runDSL[Rep = BindOrFail[Stringify]](implication) apply ("a" |-> ⊤) match {
      case (unbound: Set[String]) =>
        println(s"Unbound variables: $unbound")
      case (s: Stringify) =>
        runDSL[Rep = Stringify](s) apply System.out
    println
    }

    runDSL[Rep = BindOrFail[Stringify]](implication) apply (("a" |-> ⊤[Stringify]) ++ ("b" |-> ⊥)) match {
      case (unbound: Set[String]) =>
        println(s"Unbound variables: $unbound")
      case (s: Stringify) =>
        runDSL[Rep = Stringify](s) apply System.out
    println
    }
  }
}
