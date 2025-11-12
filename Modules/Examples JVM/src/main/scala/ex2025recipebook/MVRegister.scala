package ex2025recipebook


import rdts.base.{Bottom, Decompose, DecoratedLattice, Historized, Lattice, LocalUid}
import rdts.time.{Dot, Dots}

/** An MultiVersionRegister (Multi-Value Register) is a Delta CRDT modeling a register.
  *
  * In the absence of concurrent writes, the MultiVersionRegister is either empty or holds one value.
  * When multiple values are written concurrently, reading the MultiVersionRegister returns a set holding all these values.
  *
  * This implementation produces larger deltas when writing values into the register
  */
case class MVRegister[A](repr: Map[Dot, A], removed: Dots) {

  def read: Set[A] = compact.repr.values.toSet

  def compact: MVRegister[A] = MVRegister(repr.filter((d, _) => !removed.contains(d)), removed)

  def write(v: A)(using LocalUid): MVRegister[A] = {
    val nextDot       = observed.nextDot(LocalUid.replicaId)
    MVRegister(
      Map(nextDot -> v),
      observed
    )
  }

  def clear(): MVRegister[A] =
    MVRegister(
      Map.empty,
      Dots.from(repr.keySet)
    )

  def observed: Dots = repr.foldLeft(removed)((dots, entry) => dots.add(entry._1))
}

object MVRegister {

  def of[A](a: A)(using LocalUid): MVRegister[A] = empty.write(a)

  given bottomInstance[A]: Bottom[MVRegister[A]] = Bottom.derived
  def empty[A]: MVRegister[A]                    = Bottom.empty

  given decomposeInstance[A]: Decompose[MVRegister[A]] = {
    given Decompose[A] = Decompose.atomic
    Decompose.derived
  }

  given lattice[A]: Lattice[MVRegister[A]] =
    given Lattice[A] = Lattice.assertEquals
    val decorated    = Lattice.derived[MVRegister[A]]
    DecoratedLattice.filter(decorated) { (base, other) =>
      base.copy(repr = base.repr.filter((k, _) => !other.removed.contains(k)))
    }

  /** The buffered delta is redundant if it happened before the delta
   * -> the key/dot of the write operation in the buffered delta is removed by the delta
   */
  given historized[A]: Historized[MVRegister[A]] = (delta, bufferedDelta) =>
    delta.removed.contains(bufferedDelta.observed)

}
