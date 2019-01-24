@FunctionalInterface
trait RunDSL[Rep, Res] {
  def runDSL(a: Rep): Res
}

trait TruthValues[B] {
  def ⊤ : B
  def ⊥ : B
}

@FunctionalInterface
trait And[B] {
  def and(lhs: B, rhs: B): B
}

@FunctionalInterface
trait Or[B] {
  def or(lhs: B, rhs: B): B
}

@FunctionalInterface
trait Not[B] {
  def not(lhs: B): B
}

object StringifyContext {
  import scala.language.implicitConversions

  opaque type Stringify = Appendable => Unit

  object Stringify {
    def (lhs: Stringify) ++ (rhs: Stringify): Stringify =
      a => { lhs(a); rhs(a) }
    def (s: String) a: Stringify = _ append s

    implicit object StringifyTruthValues extends TruthValues[Stringify] {
      override def ⊤ : Stringify = "⊤".a
      override def ⊥ : Stringify = "⊥".a
    }
    implicit object StringifyAnd extends And[Stringify] {
      override def and(lhs: Stringify, rhs: Stringify): Stringify =
      "and(".a ++ lhs ++ ",".a ++ rhs ++ ")".a
    }
    implicit object StringifyOr extends Or[Stringify] {
      override def or(lhs: Stringify, rhs: Stringify): Stringify =
        "or(".a ++ lhs ++ ",".a ++ rhs ++ ")".a
    }
    implicit object StringifyNot extends Not[Stringify] {
      override def not(lhs: Stringify): Stringify =
      "not(".a ++ lhs ++ ")".a
    }

    def run(s: Stringify, a: Appendable): Unit = s(a)
  }

  implicit object RunStringify extends RunDSL[Stringify, Appendable => Unit] {
    override def runDSL(s: Stringify) = Stringify.run(s, _)
  }
}

import StringifyContext._

object BooleanContext {
  opaque type Bool = Boolean

  object Bool {
    implicit object BooleanTruthValues extends TruthValues[Bool] {
      override def ⊤ : Bool = true
      override def ⊥ : Bool = false
    }
    implicit object BooleanAnd extends And[Bool] {
      override def and(lhs: Bool, rhs: Bool): Bool= lhs && rhs
    }
    implicit object BooleanOr extends Or[Bool] {
      override def or(lhs: Bool, rhs: Bool): Bool = lhs || rhs
    }
    implicit object BooleanNot extends Not[Bool] {
      override def not(lhs: Bool): Bool = !lhs
    }
    private [BooleanContext] def run(b: Bool): Boolean = b
  }

  implicit object RunBool extends RunDSL[Bool, Boolean] {
    def runDSL(b: Bool): Boolean = Bool.run(b)
  }
}

import BooleanContext._

object NNFContext {
  opaque type NNF[T] = Boolean => T

  object NNF {
    implicit def NNFNot[B]: Not[NNF[B]] = (lhs: NNF[B]) => ctx => lhs(!ctx)
    implicit def NNFAnd[B](implicit A: And[B], O: Or[B]): And[NNF[B]] = (lhs: NNF[B], rhs: NNF[B]) => {
      case true  => A.and(lhs(true), rhs(true))
      case false => O.or(lhs(false), rhs(false))
    }
    implicit def NNFOr[B](implicit A: And[B], O: Or[B]): Or[NNF[B]] = (lhs: NNF[B], rhs: NNF[B]) => {
      case true  => O.or(lhs(true), rhs(true))
      case false => A.and(lhs(false), rhs(false))
    }
    implicit def NNFTruthValues[B](implicit B: TruthValues[B]): TruthValues[NNF[B]] = new {
      def ⊤ : NNF[B] = {
        case true  => B.⊤
        case false => B.⊥
      }
      def ⊥ : NNF[B] = {
        case true  => B.⊥
        case false => B.⊤
      }
    }

    private [NNFContext] def run[B](b: NNF[B]): B = b(true)
  }

  implicit def RunNNF[B]: RunDSL[NNF[B], B] = new {
    override def runDSL(b: NNF[B]): B = NNF.run(b)
  }
}

import NNFContext._

object Main {
  // syntax, because there isn't yet a nice way to deal with this without boilerplate - hint, hint
  inline def runDSL[Rep, Res](a: Rep)(implicit R: RunDSL[Rep, Res]): Res = R.runDSL(a)

  inline def ⊤ [B](implicit B: TruthValues[B]): B = B.⊤
  inline def ⊥ [B](implicit B: TruthValues[B]): B = B.⊥
  inline def and[B](lhs: B, rhs: B)(implicit B: And[B]): B = B.and(lhs, rhs)
  inline def or[B](lhs: B, rhs: B)(implicit B: Or[B]): B = B.or(lhs, rhs)
  inline def not[B](lhs: B)(implicit B: Not[B]): B = B.not(lhs)

  // Some logical statements
  // These are here as defs rather than inline as expressions because type inference works for the instances within
  // defs but fails for expressions, due to the interaction of how implicits are prioritised in scope and how much
  // type information is used from the call context
  def andTF[B](implicit A: And[B], T: TruthValues[B]): B = and(⊤, ⊥)
  def orTF[B](implicit O: Or[B], T: TruthValues[B]): B = or(⊤, ⊥)
  def notF[B](implicit N: Not[B], T: TruthValues[B]): B = not(⊥)
  def notAndTNotF[B](implicit A: And[B], N: Not[B], T: TruthValues[B]): B = not(and(⊤, not(⊥)))

  def main(args: Array[String]): Unit = {
    // this won't compile because of type inference being not good enough
//    runDSL[Rep = Stringify](and(⊤, ⊥))

    // these all will
    runDSL[Rep = Stringify](andTF) apply System.out ; println
    println(runDSL[Rep = Bool](andTF))
    println(runDSL[Rep = Bool](orTF))
    println(runDSL[Rep = Bool](notF))

    runDSL[Rep = Stringify](notAndTNotF) apply System.out ; println
    println(runDSL[Rep = Bool](notAndTNotF))
    runDSL[Rep = Stringify](runDSL[Rep = NNF[Stringify]](notAndTNotF)) apply System.out ; println
  }
}
