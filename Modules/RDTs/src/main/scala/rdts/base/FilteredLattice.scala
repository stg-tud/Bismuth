package rdts.base

/** Decorates an existing lattice to filter the values before merging them.
  * Warning: Decoration breaks when the decorated lattice has overridden methods except merge and decompose, or uses merge from within merge/decompose.
  */
trait FilteredLattice[A](decorated: Lattice[A]) extends Lattice[A] {
  def filter(base: A, other: A): A

  def merge(left: A, right: A): A =
    val filteredLeft  = filter(left, right)
    val filteredRight = filter(right, left)
    decorated.merge(filteredLeft, filteredRight)
}
