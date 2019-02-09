// A representation that can be 'run' to convert an intermediate representation into a final result.
@FunctionalInterface
trait RunDSL[Rep, Res] {
  def runDSL(a: Rep): Res
}

object RunDSL {
  implied RunIdentity[R] for RunDSL[R, R] = identity

  def (rep: Rep) run[Rep, Res] given (r: RunDSL[Rep, Res]): Res = r.runDSL(rep)
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
