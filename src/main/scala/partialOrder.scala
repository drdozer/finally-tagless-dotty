@FunctionalInterface
trait Lt[P, B] {
  def lt(lhs: P, rhs: P): B
}

object Lt {
  implied IntLt[B] given (t: TruthValues[B]) for Lt[Int, B] = (lhs, rhs) => t.of(lhs < rhs)
  implied NNFLt[P, B] given (t: Lt[P, B], n: Gteq[P, B]) for Lt[P, NNF[B]] = (lhs, rhs) => new {
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
  implied NNFLteq[P, B] given (t: Lteq[P, B], n: Gt[P, B]) for Lteq[P, NNF[B]] = (lhs, rhs) => new {
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
  implied NNFGteq[P, B] given (t: Gteq[P, B], n: Lt[P, B]) for Gteq[P, NNF[B]] = (lhs, rhs) => new {
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
  implied NNFGt[P, B] given (t: Gt[P, B], n: Lteq[P, B]) for Gt[P, NNF[B]] = (lhs, rhs) => new {
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
  implied NNFEq[P, B] given (t: Eq[P, B], n: Neq[P, B]) for Eq[P, NNF[B]] = (lhs, rhs) => new {
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
  implied NNFNeq[P, B] given (t: Neq[P, B], n: Eq[P, B]) for Neq[P, NNF[B]] = (lhs, rhs) => new {
    override def terminal: B = t.neq(lhs, rhs)
    override def negationOf: B = n.eq(lhs, rhs)
  }
}


opaque type ONF[P] = P

object ONF {
  implied LtTotalOrder[P, B] given (pb: Lt[P, B]) for Lt[ONF[P], B] = (lhs, rhs) => pb.lt(lhs, rhs)
  implied LteqTotalOrder[P, B] given (l: Lt[P, B], e: Eq[P, B], o: Or[B]) for Lteq[ONF[P], B] = (lhs, rhs) =>
    o.or(
      l.lt(lhs, rhs),
      e.eq(lhs, rhs)
    )
  implied GteqTotalOrder[P, B] given (l: Lt[P, B], e: Eq[P, B], o: Or[B]) for Gteq[ONF[P], B] = (lhs, rhs) =>
    o.or(
      l.lt(rhs, lhs),
      e.eq(rhs, lhs)
    )
  implied GtOrder[P, B] given (pb: Lt[P, B]) for Gt[ONF[P], B] = (lhs, rhs) => pb.lt(rhs, lhs)
  implied EqTotalOrder[P, B] given (e: Eq[P, B]) for Eq[ONF[P], B] = (lhs, rhs) => e.eq(lhs, rhs)
  implied NeqTotalOrder[P, B] given (l: Lt[P, B], o: Or[B]) for Neq[ONF[P], B] = (lhs, rhs) =>
    o.or(
      l.lt(lhs, rhs),
      l.lt(rhs, lhs)
    )
}


case class PartialOrderModel[P](lt: Set[(P, P)], eq: Set[(P, P)])
object PartialOrderModel {
  def empty[P]: PartialOrderModel[P] = PartialOrderModel(Set.empty, Set.empty)
}
opaque type PartialOrder[P] = PartialOrderModel[P] => PartialOrderModel[P]

object PartialOrder {
  implied LtPartialOrder[P] for Lt[P, PartialOrder[P]] = (lhs, rhs) =>
    po => po.copy(lt = po.lt + ((lhs, rhs)))
  implied EqPartialOrder[P] for Eq[P, PartialOrder[P]] = (lhs, rhs) =>
    po => po.copy(eq = po.eq + ((lhs, rhs)))
  implied AndPartialOrder[P] for And[PartialOrder[P]] = (lhs, rhs) =>
    lhs andThen rhs

  implied RunPartialOrder[P] for RunDSL[PartialOrder[P], PartialOrderModel[P]] = _.apply(PartialOrderModel.empty)
}


//enum OrdAss[A] {
//  case LT(lhs: A, rhs: A)
//  case EQ(lhs: A, rhs: A)
//}
//
//trait OrderingAssertion[V, Lt, Eq] {
//  def assertLT(lhs: V, rhs: V): Lt
//  def assertEQ(lhs: V, rhs: V): Eq
//}
//
//trait OrderingInference[Lt, Eq) {
//  // ltEqLhs:  a < b, a = x |- x < b
//  def ltEqLhs(ltAB: Lt, eqAX: Eq): Lt
//
//  // ltEqRhs:  a < b, b = x |- a < x
//  def ltEqRhs(ltAB: Lt, eqBX: Eq): Lt
//
//  // eqSelf:                |- a = a
//  // eqSub:    a = b, f     |- substitute a for b in f
//  // eqSym:    a = b        |- b = a
//  // eqTrans:  a = b, b = x |- a = x
//  // eqTrans2: a = b, a = x |- x = b // eqSym & eqTrans
//
//}
///*
//
//inference:
//  ltEqRhs:  a < b, b = x |- a < x
//  eqSelf:                |- a = a
//  eqSub:    a = b, f     |- substitute a for b in f
//  eqSym:    a = b        |- b = a
//  eqTrans:  a = b, b = x |- a = x
//  eqTrans2: a = b, a = x |- x = b // eqSym & eqTrans
//
//conflict:
//  ltNotEq: a < b /- a = b
//  selfNotLt:     /- a < a
//  eqNotLt: a = b /- a < b   // selfNotLit & (a = b, substitute a for b)
//  eqNotLt2: b = a /- a < b  // selfNotLit &
//
// */