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
