package ct

// A representation that can be 'run' to convert an intermediate representation into a final result.
@FunctionalInterface
trait RunDSL[Rep, Res] {
  def runDSL(a: Rep): Res
}

object RunDSL {
  implied RunIdentity[R] for RunDSL[R, R] = identity
}

implied RunDSLSyntax {
  def (rep: Rep) run[Rep, Res] given (r: RunDSL[Rep, Res]): Res = r.runDSL(rep)
}


opaque type Stringify = Appendable => Unit

object Stringify {

  import Semigroup._

  def apply(a: Appendable => Unit): Stringify = a

  def run(s: Stringify, a: Appendable): Unit = s(a)
}

implied StringifySyntax {
  def (s: String) a: Stringify = Stringify(_ append s)
}

implied StringifySemigroup for Semigroup[Stringify] {
  override def append(lhs: Stringify, rhs: Stringify): Stringify =
  Stringify(a => { Stringify.run(lhs, a) ; Stringify.run(rhs, a) })
}

implied RunStringify for RunDSL[Stringify, Appendable => Unit] =
  s => Stringify.run(s, _)
