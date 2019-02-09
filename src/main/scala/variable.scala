
@FunctionalInterface
trait Variable[B] {
  def variable(name: String): B
}


type MapBinding[B] = (String, given TruthValues[B] => B)
opaque type MapBindings[B] = List[MapBinding[B]]

def (name: String) |-> [B](value: given TruthValues[B] => B): MapBindings[B] = MapBindings.bind(name, value)

object MapBindings {
  def empty[B]: MapBindings[B] = Nil
  def bind[B](binding: MapBinding[B]): MapBindings[B] = binding :: Nil

  implied MapBindingsSemigroup[B] for Semigroup[MapBindings[B]] = _ ++ _

  def lookup[B](mb: MapBindings[B], name: String): Option[given TruthValues[B] => B] = mb.find(_._1 == name).map(_._2)
}



opaque type BoundOrFail[+B] =
  Set[String] | // evaluate to the set for unbound variable names
    B // or a value if all are bound

object BoundOrFail {
  def (s: Set[String]) unbound[B]: BoundOrFail[Nothing] = s
  def (b: B) bound[B]: BoundOrFail[B] = b

  def (b: BoundOrFail[B]) fold[B, C](onSuccess: B => C, onFailure: Set[String] => C): C = b match {
    case f: Set[String] => onFailure(f)
    case s: B => onSuccess(s)
  }

}


opaque type BindOrFail[B] = MapBindings[B] => BoundOrFail[B]

object BindOrFail {
  import BoundOrFail._

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

