// A representation that can be 'run' to convert an intermediate representation into a final result.
@FunctionalInterface
trait RunDSL[Rep, Res] {
  def runDSL(a: Rep): Res
}

object RunDSL {
  instance def RunIdentity[R]: RunDSL[R, R] = identity
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


trait Snoccable[C, E] {
  def (col: C) snoc (e: E): C
}

trait Consable[C, E] {
  def (e: E) cons (col: C): C
}

