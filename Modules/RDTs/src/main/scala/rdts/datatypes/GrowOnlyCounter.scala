package rdts.datatypes

import rdts.base.{Bottom, Decompose, Historized, Lattice, LocalUid, Uid}
import rdts.time.Dots

case class GrowOnlyCounter(inner: Map[Uid, Int]) {
  lazy val value: Int = inner.valuesIterator.sum

  def inc()(using localReplicaId: LocalUid): GrowOnlyCounter            = add(1)
  def add(amount: Int)(using localReplicaId: LocalUid): GrowOnlyCounter =
     require(amount >= 0, "may not decrease counter")
     GrowOnlyCounter(Map(localReplicaId.uid -> (inner.getOrElse(localReplicaId.uid, 0) + amount)))
}

/** A GCounter is a Delta CRDT modeling an increment-only counter. */
object GrowOnlyCounter {
  def zero: GrowOnlyCounter = GrowOnlyCounter(Map.empty)

  given bottom: Bottom[GrowOnlyCounter] = Bottom.derived

  given lattice: Lattice[GrowOnlyCounter] =
     given Lattice[Int] = math.max
     Lattice.derived

  given decompose: Decompose[GrowOnlyCounter] =
     given Decompose[Int] = Decompose.atomic
     Decompose.derived

  /** the delta must contain all keys of the buffered delta, and the counter of each replica must be greater equal */
  given historized: Historized[GrowOnlyCounter] = (delta, bufferedDelta) =>
    if bufferedDelta.delta.inner.keys.forall(k =>
         delta.inner.contains(k) && bufferedDelta.delta.inner(k) <= delta.inner(k)
       )
    then
       bufferedDelta.getAllDots
    else
       Dots.empty
}
