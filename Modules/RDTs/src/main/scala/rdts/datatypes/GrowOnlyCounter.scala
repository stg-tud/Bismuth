package rdts.datatypes

import rdts.base.*

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

  given lattice: Lattice[GrowOnlyCounter] = new Lattice[GrowOnlyCounter]:
      override def merge(left: GrowOnlyCounter, right: GrowOnlyCounter): GrowOnlyCounter =
        GrowOnlyCounter(right.inner.foldLeft(left.inner) {
          case (current, (key, l)) =>
            current.updatedWith(key) {
              case Some(r) => Some(if r > l then r else l)
              case None    => Some(l)
            }
        })

  given decompose: Decompose[GrowOnlyCounter] =
      given Decompose[Int] = Decompose.atomic
      Decompose.derived

  /** the delta must contain all replica ids of the buffered delta,
    * and the counter of each replica must be greater equal
    */
  given historized: Historized[GrowOnlyCounter] = (delta, bufferedDelta) =>
    bufferedDelta.inner.forall((uid, counter) => counter <= delta.inner.getOrElse(uid, -1))
}
