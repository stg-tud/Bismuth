package rdts.datatypes

import rdts.base.{Bottom, Decompose, Lattice, LocalUid}
import rdts.dotted.HasDots.mapInstance
import rdts.dotted.{FilteredLattice, HasDots}
import rdts.time.{Dot, Dots}

/** An MultiVersionRegister (Multi-Value Register) is a Delta CRDT modeling a register.
  *
  * In the absence of concurrent writes, the MultiVersionRegister is either empty or holds one value.
  * When multiple values are written concurrently, reading the MultiVersionRegister returns a set holding all these values.
  */
case class MultiVersionRegister[A](repr: Map[Dot, A], removed: Dots) {

  def read: Set[A] = compact.repr.values.toSet

  def compact: MultiVersionRegister[A] = MultiVersionRegister(repr.filter((d, _) => !removed.contains(d)), removed)

  def write(using LocalUid)(v: A): MultiVersionRegister[A] = {

    val containedDots = Dots.from(repr.keys)
    val nextDot       = (removed.union(containedDots)).nextDot(LocalUid.replicaId)

    MultiVersionRegister(
      Map(nextDot -> v),
      containedDots
    )
  }

  def clear(): MultiVersionRegister[A] =
    MultiVersionRegister(
      Map.empty,
      Dots.from(repr.keySet)
    )
}

object MultiVersionRegister {

  given bottomInstance[A]: Bottom[MultiVersionRegister[A]] = Bottom.derived
  def empty[A]: MultiVersionRegister[A]                    = Bottom.empty

  given decomposeInstance[A]: Decompose[MultiVersionRegister[A]] = {
    given Decompose[A] = Decompose.atomic
    Decompose.derived
  }

  given lattice[A]: Lattice[MultiVersionRegister[A]] =
    given Lattice[A] = Lattice.assertEquals
    val decorated    = Lattice.derived[MultiVersionRegister[A]]
    new FilteredLattice[MultiVersionRegister[A]](decorated) {
      override def filter(base: MultiVersionRegister[A], other: MultiVersionRegister[A]): MultiVersionRegister[A] =
        base.copy(repr = base.repr.removeDots(other.removed).getOrElse(Map.empty))
    }

}
