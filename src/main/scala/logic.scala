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

trait TruthValues[B] {
  def ⊤ : B
  def ⊥ : B
  inline def of(b: Boolean): B = if(b) ⊤ else ⊥
}


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
  def unwrap(b: Bool): Boolean = b
}

implied RunBool for RunDSL[Bool, Boolean] = Bool.unwrap
