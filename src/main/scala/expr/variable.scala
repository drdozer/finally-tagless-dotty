package expr

import ct._
import implied ct._

@FunctionalInterface
trait Variable[B] {
  def variable(name: String): B
}

object Variable {
  implied StringifyVariable for Variable[Stringify] {
    override def variable(name: String): Stringify = "?".a ++ name.a
  }
}

type MapBinding[B] = (String, given TruthValues[B] => B)
opaque type MapBindings[B] = List[MapBinding[B]]

def (name: String) |-> [B](value: given TruthValues[B] => B): MapBindings[B] = MapBindings.bind(name, value)

object MapBindings {
  def empty[B]: MapBindings[B] = Nil
  def bind[B](binding: MapBinding[B]): MapBindings[B] = binding :: Nil

  implied MapBindingsSemigroup[B] for Semigroup[MapBindings[B]] = _ ++ _

  def lookup[B](mb: MapBindings[B], name: String): Option[given TruthValues[B] => B] =
    mb.find(_._1 == name).map(_._2)
}

trait BoundOrUnbound[B, BU] {
  def bound(b: B): BU
  def unbound(reason: Set[String]): BU
}

@FunctionalInterface
trait BindingResult[B] {
  def apply[BU](bu: BoundOrUnbound[B, BU]): BU
}

object BindingResult {
  def (b: B) bound[B]: BindingResult[B] = new {
    override def apply[BU](bu: BoundOrUnbound[B, BU]): BU = bu.bound(b)
  }
  def (r: Set[String]) unbound[B]: BindingResult[B] = new {
    override def apply[BU](bu: BoundOrUnbound[B, BU]): BU = bu.unbound(r)
  }
}

opaque type BindOrFail[B] = MapBindings[B] => BindingResult[B]

object BindOrFail {
  import BindingResult._

  implied BindVariable[B] given TruthValues[B] for Variable[BindOrFail[B]] =
    name => MapBindings.lookup(_, name) match {
      case Some(b) => b.bound
      case None => Set(name).unbound
    }


  def BindSemigroup[B](S: Semigroup[B]): Semigroup[BindOrFail[B]] =
    (lhs, rhs) => b => lhs(b)(new {
      override def bound(bl: B) = rhs(b)(new {
        override def bound(br: B) = S.append(bl, br).bound
        override def unbound(ur: Set[String]) = ur.unbound
      })
      override def unbound(ul: Set[String]) = rhs(b)(new {
        override def bound(br: B) = br.bound
        override def unbound(ur: Set[String]) = (ul ++ ur).unbound
      })
    })

  implied BindAnd[B] given (A: And[B]) for And[BindOrFail[B]] = BindSemigroup(A.asSemigroup).append
  implied BindOr[B] given (O: Or[B]) for Or[BindOrFail[B]] = BindSemigroup(O.asSemigroup).append

  implied BindNot[B] given (N: Not[B]) for Not[BindOrFail[B]] =
    lhs => b => lhs(b)(new {
      override def bound(b: B) = N.not(b).bound
      override def unbound(u: Set[String]) = u.unbound
    })

  implied BindTruthValue[B] given (T: TruthValues[B]) for TruthValues[BindOrFail[B]] {
    override def ⊤ : BindOrFail[B] = _ => T.⊤.bound
    override def ⊥ : BindOrFail[B] = _ => T.⊥.bound
  }

  def unwrap[B](bof: BindOrFail[B]): MapBindings[B] => BindingResult[B] = bof
}

implied RunBindOrFail[B] for RunDSL[BindOrFail[B], MapBindings[B] => BindingResult[B]] = BindOrFail.unwrap


opaque type BindIfPossible[B] = MapBindings[B] => B


object BindIfPossible {
  def unwrap[B](b: BindIfPossible[B]): MapBindings[B] => B = b

  implied BindVariable[B] given (T: TruthValues[B], V: Variable[B]) for Variable[BindIfPossible[B]] =
    name => MapBindings.lookup(_, name) match {
      case Some(b) => b
      case None => V.variable(name)
    }

  def BindSemigroup[B](S: Semigroup[B]): Semigroup[BindIfPossible[B]] =
    (lhs, rhs) => b => S.append(lhs(b), rhs(b))

  implied BindAnd[B] given (A: And[B]) for And[BindIfPossible[B]] = BindSemigroup(A.asSemigroup).append
  implied BindOr [B] given (O: Or [B]) for Or [BindIfPossible[B]] = BindSemigroup(O.asSemigroup).append
  implied BindNot[B] given (N: Not[B]) for Not[BindIfPossible[B]] = lhs => b => N.not(lhs(b))

  implied BindTruthValues[B] given (T: TruthValues[B]) for TruthValues[BindIfPossible[B]] {
    override def ⊤ : BindIfPossible[B] = _ => T.⊤
    override def ⊥ : BindIfPossible[B] = _ => T.⊥
  }
}

implied RunBindIfPossible[B] for RunDSL[BindIfPossible[B], MapBindings[B] => B] = BindIfPossible.unwrap

