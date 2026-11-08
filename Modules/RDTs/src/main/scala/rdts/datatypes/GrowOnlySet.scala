package rdts.datatypes

import rdts.base.{Bottom, Lattice}

case class GrowOnlySet[A](set: Set[A]) {

  def add(a: A): GrowOnlySet[A] = GrowOnlySet(Set(a))

}

object GrowOnlySet {

  def empty[A]: GrowOnlySet[A] = GrowOnlySet(Set.empty[A])

  def apply[A](a: A): GrowOnlySet[A] = GrowOnlySet(Set(a))

  given bottom[A]: Bottom[GrowOnlySet[A]] = Bottom.derived

  given lattice[A]: Lattice[GrowOnlySet[A]] = Lattice.derived

}
