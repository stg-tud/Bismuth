package rdts.base

import scala.annotation.nowarn

/** Decorates an existing lattice to filter the values before merging them.
  * Warning: Decoration breaks when the decorated lattice has overridden methods except merge and decompose, or uses merge from within merge/decompose.
  */
class DecoratedLattice[A](decorated: Lattice[A]) extends Lattice[A] {
  def filter(base: A, other: A): A = base

  def compact(merged: A): A = merged

  def merge(left: A, right: A): A =
    val filteredLeft  = filter(left, right)
    val filteredRight = filter(right, left)
    compact:
      decorated.merge(filteredLeft, filteredRight)
}

object DecoratedLattice {
  inline def filter[A](inline decorated: Lattice[A])(inline filt: (A, A) => A): DecoratedLattice[A] =
    new DecoratedLattice[A](decorated) {
      override def filter(base: A, other: A): A = filt(base, other)
    }: @nowarn("msg=class definition will be duplicated")

  inline def compact[A](inline decorated: Lattice[A])(inline comp: A => A): DecoratedLattice[A] =
    new DecoratedLattice[A](decorated) {
      override def compact(merged: A): A = comp(merged)
    }: @nowarn("msg=class definition will be duplicated")
}
