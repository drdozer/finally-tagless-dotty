// A representation that can be 'run' to convert an intermediate representation into a final result.
@FunctionalInterface
trait RunDSL[Rep, Res] {
  def runDSL(a: Rep): Res
}



// A binary, associative operation.
@FunctionalInterface
trait Semigroup[S] {
  def append(lhs: S, rhs: S): S
}

object Semigroup {
  def (lhs: S) ++ [S](rhs: S) with (S: Semigroup[S]): S = S.append(lhs, rhs)

  trait RewriteToRight[S] {
    def toLeftOf(rhs: S): S
    def terminal: S
  }

  @FunctionalInterface
  def RewriteToRightTerminal[S](term: S) with (S: Semigroup[S]): RewriteToRight[S] = new {
    override def toLeftOf(rhs: S): S = S.append(terminal, rhs)
    override def terminal: S = term
  }

  instance RewriteToRightAppend[S] of Semigroup[RewriteToRight[S]] {
    override def append(lhs: RewriteToRight[S], rhs: RewriteToRight[S]): RewriteToRight[S] = new {
      override def terminal: S = lhs.toLeftOf(rhs.terminal)
      override def toLeftOf(r: S): S = lhs.toLeftOf(rhs.toLeftOf(r))
    }
  }

  instance RunRewriteToRight[S] of RunDSL[RewriteToRight[S], S] {
    override def runDSL(r: RewriteToRight[S]): S = r.terminal
  }

  trait RewriteToLeft[S] {
    def toRightOf(lhs: S): S
    def terminal: S
  }

  @FunctionalInterface
  def RewriteToLeftTerminal[S](term: S) with (S: Semigroup[S]): RewriteToLeft[S] = new {
    override def toRightOf(lhs: S): S = S.append(lhs, terminal)
    override def terminal: S = term
  }

  instance RewriteToLeftAppend[S] of Semigroup[RewriteToLeft[S]] {
    override def append(lhs: RewriteToLeft[S], rhs: RewriteToLeft[S]): RewriteToLeft[S] = new {
      override def terminal: S = rhs.toRightOf(lhs.terminal)
      override def toRightOf(l: S): S = rhs.toRightOf(lhs.toRightOf(l))
    }
  }

  instance RunRewriteToLeft[S] of RunDSL[RewriteToLeft[S], S] {
    override def runDSL(r: RewriteToLeft[S]): S = r.terminal
  }
}

import Semigroup._


@FunctionalInterface
trait AsSemigroup[S[_]] {
  def (s: S[T]) asSemigroup[T]: Semigroup[T]
}


trait Distributive[S] {
  def add(lhs: S, rhs: S): S
  def mult(lhs: S, rhs: S): S
}

object Distributive {
  def apply[S](addS: (S, S) => S, multS: (S, S) => S): Distributive[S] = new {
    override def add(lhs: S, rhs: S): S = addS(lhs, rhs)
    override def mult(lhs: S, rhs: S): S = multS(lhs, rhs)
  }

  opaque type Add[S] = S
  object Add {
    def apply[S](s: S): Add[S] = s
    def (a: Add[S]) unwrap[S]: S = a
  }
  instance AddSemigroup[S] with (S: Distributive[S]) of Semigroup[Add[S]] {
    import Add.unwrap
    override def append(lhs: Add[S], rhs: Add[S]): Add[S] = Add(S.add(lhs.unwrap, rhs.unwrap))
  }
  instance RunAdd[S] of RunDSL[Add[S], S] {
    import Add.unwrap
    override def runDSL(a: Add[S]): S = a.unwrap
  }


  opaque type Mult[S] = S
  object Mult {
    def apply[S](s: S): Mult[S] = s
    def (a: Mult[S]) unwrap[S]: S = a
  }
  instance MultSemigroup[S] with (S: Distributive[S]) of Semigroup[Mult[S]] {
    import Mult.unwrap
    override def append(lhs: Mult[S], rhs: Mult[S]): Mult[S] = Mult(S.mult(lhs.unwrap, rhs.unwrap))
  }
  instance RunMult[S] of RunDSL[Mult[S], S] {
    import Mult.unwrap
    override def runDSL(a: Mult[S]): S = a.unwrap
  }

  // x * (y + z) => (x * y) + (x * z)
  trait DistributeLeft[S] {
    def toRightOf(lhs: S): S
    def terminal: S
  }
  def LeftDistributedTerminal[S](t: S) with (S: Distributive[S]): DistributeLeft[S] = new {
    override def toRightOf(lhs: S): S = S.mult(lhs, t)
    override def terminal: S = t
  }
  instance LeftDistributingSemiRng[S] with (S: Distributive[S]) of Distributive[DistributeLeft[S]] {
    override def add(lhs: DistributeLeft[S], rhs: DistributeLeft[S]): DistributeLeft[S] = new {
      override def terminal: S = S.add(lhs.terminal, rhs.terminal)
      override def toRightOf(l: S): S = S.add(lhs.toRightOf(l), rhs.toRightOf(l))
    }

    override def mult(lhs: DistributeLeft[S], rhs: DistributeLeft[S]): DistributeLeft[S] = new {
      override def terminal: S = rhs.toRightOf(lhs.terminal)
      override def toRightOf(l: S): S = rhs.toRightOf(lhs.toRightOf(l))
    }
  }
  instance RunDistributeLeft[S] of RunDSL[DistributeLeft[S], S] {
    override def runDSL(d: DistributeLeft[S]): S = d.terminal
  }

  // (x + y) * z => (x * z) + (y * z)
  trait DistributeRight[S] {
    def toLeftOf(rhs: S): S
    def terminal: S
  }
  def RightDistributedTerminal[S](t: S) with (S: Distributive[S]): DistributeRight[S] = new {
    override def toLeftOf(lhs: S): S = S.mult(lhs, t)
    override def terminal: S = t
  }
  instance RightDistributingSemiRng[S] with (S: Distributive[S]) of Distributive[DistributeRight[S]] {
    override def add(lhs: DistributeRight[S], rhs: DistributeRight[S]): DistributeRight[S] = new {
      override def terminal: S = S.add(lhs.terminal, rhs.terminal)
      override def toLeftOf(r: S): S = S.add(lhs.toLeftOf(r), rhs.toLeftOf(r))
    }

    override def mult(lhs: DistributeRight[S], rhs: DistributeRight[S]): DistributeRight[S] = new {
      override def terminal: S = rhs.toLeftOf(lhs.terminal)
      override def toLeftOf(r: S): S = lhs.toLeftOf(rhs.toLeftOf(r))
    }
  }
  instance RunDistributeRight[S] of RunDSL[DistributeRight[S], S] {
    override def runDSL(d: DistributeRight[S]): S = d.terminal
  }
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
