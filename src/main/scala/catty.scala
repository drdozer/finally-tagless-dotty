// A binary, associative operation.
@FunctionalInterface
trait Semigroup[S] {
  def append(lhs: S, rhs: S): S
}

object Semigroup {
  def (lhs: S) ++ [S](rhs: S) given (S: Semigroup[S]): S = S.append(lhs, rhs)

  trait RewriteToRight[S] {
    def toLeftOf(rhs: S): S
    def terminal: S
  }

  @FunctionalInterface
  def RewriteToRightTerminal[S](term: S) given (S: Semigroup[S]): RewriteToRight[S] = new {
    override def toLeftOf(rhs: S): S = S.append(terminal, rhs)
    override def terminal: S = term
  }

  implied RewriteToRightAppend[S] for Semigroup[RewriteToRight[S]] {
    override def append(lhs: RewriteToRight[S], rhs: RewriteToRight[S]): RewriteToRight[S] = new {
      override def terminal: S = lhs.toLeftOf(rhs.terminal)
      override def toLeftOf(r: S): S = lhs.toLeftOf(rhs.toLeftOf(r))
    }
  }

  implied RunRewriteToRight[S] for RunDSL[RewriteToRight[S], S] = _.terminal


  trait RewriteToLeft[S] {
    def toRightOf(lhs: S): S
    def terminal: S
  }

  @FunctionalInterface
  def RewriteToLeftTerminal[S](term: S) given (S: Semigroup[S]): RewriteToLeft[S] = new {
    override def toRightOf(lhs: S): S = S.append(lhs, terminal)
    override def terminal: S = term
  }

  implied RewriteToLeftAppend[S] for Semigroup[RewriteToLeft[S]] {
    override def append(lhs: RewriteToLeft[S], rhs: RewriteToLeft[S]): RewriteToLeft[S] = new {
      override def terminal: S = rhs.toRightOf(lhs.terminal)
      override def toRightOf(l: S): S = rhs.toRightOf(lhs.toRightOf(l))
    }
  }

  implied RunRewriteToLeft[S] for RunDSL[RewriteToLeft[S], S] = _.terminal
}


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
    implied AddSemigroup[S] given (S: Distributive[S]) for Semigroup[Add[S]] {
      override def append(lhs: Add[S], rhs: Add[S]): Add[S] = Add(S.add(lhs.unwrap, rhs.unwrap))
    }
    implied RunAdd[S] for RunDSL[Add[S], S] = _.unwrap
  }


  opaque type Mult[S] = S
  object Mult {
    def apply[S](s: S): Mult[S] = s
    def (a: Mult[S]) unwrap[S]: S = a
    implied MultSemigroup[S] given (S: Distributive[S]) for Semigroup[Mult[S]] {
      override def append(lhs: Mult[S], rhs: Mult[S]): Mult[S] = Mult(S.mult(lhs.unwrap, rhs.unwrap))
    }
    implied RunMult[S] for RunDSL[Mult[S], S] = _.unwrap
  }

}

// x * (y + z) => (x * y) + (x * z)
trait DistributeLeft[S] {
  def toRightOf(lhs: S): S
  def terminal: S
}
object DistributeLeft {
  def LeftDistributedTerminal[S](t: S) given (S: Distributive[S]): DistributeLeft[S] = new {
    override def toRightOf(lhs: S): S = S.mult(lhs, t)
    override def terminal: S = t
  }
  implied LeftDistributingSemiRng[S] given (S: Distributive[S]) for Distributive[DistributeLeft[S]] {
    override def add(lhs: DistributeLeft[S], rhs: DistributeLeft[S]): DistributeLeft[S] = new {
      override def terminal: S = S.add(lhs.terminal, rhs.terminal)
      override def toRightOf(l: S): S = S.add(lhs.toRightOf(l), rhs.toRightOf(l))
    }

    override def mult(lhs: DistributeLeft[S], rhs: DistributeLeft[S]): DistributeLeft[S] = new {
      override def terminal: S = rhs.toRightOf(lhs.terminal)
      override def toRightOf(l: S): S = rhs.toRightOf(lhs.toRightOf(l))
    }
  }
}

implied RunDistributeLeft[S] for RunDSL[DistributeLeft[S], S] = _.terminal


// (x + y) * z => (x * z) + (y * z)
trait DistributeRight[S] {
  def toLeftOf(rhs: S): S
  def terminal: S
}

object DistributeRight {
  def RightDistributedTerminal[S](t: S) given (S: Distributive[S]): DistributeRight[S] = new {
    override def toLeftOf(lhs: S): S = S.mult(lhs, t)
    override def terminal: S = t
  }
  implied RightDistributingSemiRng[S] given (S: Distributive[S]) for Distributive[DistributeRight[S]] {
    override def add(lhs: DistributeRight[S], rhs: DistributeRight[S]): DistributeRight[S] = new {
      override def terminal: S = S.add(lhs.terminal, rhs.terminal)
      override def toLeftOf(r: S): S = S.add(lhs.toLeftOf(r), rhs.toLeftOf(r))
    }

    override def mult(lhs: DistributeRight[S], rhs: DistributeRight[S]): DistributeRight[S] = new {
      override def terminal: S = rhs.toLeftOf(lhs.terminal)
      override def toLeftOf(r: S): S = lhs.toLeftOf(rhs.toLeftOf(r))
    }
  }
}
implied RunDistributeRight[S] for RunDSL[DistributeRight[S], S] = _.terminal


//
//trait Snoccable[C, E] {
//  def (col: C) snoc (e: E): C
//}
//
//trait Consable[C, E] {
//  def (e: E) cons (col: C): C
//}

