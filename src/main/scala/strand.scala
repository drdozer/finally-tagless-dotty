
trait Strandedness[S] {
  def topStrand: S
  def bottomStrand: S
}

@FunctionalInterface
trait SameStrand[S, B] {
  def sameStrand(lhs: S, rhs: S): B
}



@FunctionalInterface
trait DifferentStrand[S, B] {
  def differentStrand(lhs: S, rhs: S): S
}
