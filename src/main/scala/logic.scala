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
}

implied RunNNF[B] for RunDSL[NNF[B], B] = _.terminal
