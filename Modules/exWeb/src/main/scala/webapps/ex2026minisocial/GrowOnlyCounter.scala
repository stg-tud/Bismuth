package webapps.ex2026minisocial

import rdts.base.{Bottom, Lattice, LocalUid, Uid}

/** A grow-only counter: a map of counts per replica, where the current value is the sum over all replicas.
  *
  * Mutations are represented as new immutable values. To make replication more efficient,
  * [[add]] only produces the changed part of the state (the updated entry for this replica).
  */
case class GrowOnlyCounter(counts: Map[Uid, Int] = Map.empty) {

  /** Add `n` to this replica's own count. The returned delta only contains this replica's updated entry. */
  def add(n: Int)(using replicaId: LocalUid): GrowOnlyCounter =
    GrowOnlyCounter(Map(replicaId.uid -> (counts.getOrElse(replicaId.uid, 0) + n)))

  /** Get the value of the counter by summing all per-replica counts. */
  def value: Int = counts.values.sum
}

object GrowOnlyCounter {

  given Lattice[GrowOnlyCounter] = {
    given Lattice[Int] = math.max
    Lattice.derived
  }

  given Bottom[GrowOnlyCounter] = Bottom.provide(zero)

  def zero: GrowOnlyCounter = GrowOnlyCounter()
}