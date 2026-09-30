package rdts.protocols

import rdts.base.{Bottom, Lattice, ReplicaId, Uid}

/** Assumes that all local modifications are totally ordered. */
case class SingleWriterWriteOnceMap[A](inner: Map[Uid, A]) {
  def write(v: A)(using replicaId: ReplicaId): SingleWriterWriteOnceMap[A] =
    if inner.contains(replicaId.uid)
    then SingleWriterWriteOnceMap.unchanged
    else SingleWriterWriteOnceMap(Map(replicaId.uid -> v))

  def read(uid: Uid): Option[A] = inner.get(uid)
}

object SingleWriterWriteOnceMap {
  given lattice[A]: Lattice[SingleWriterWriteOnceMap[A]] = {
    given Lattice[A] = Lattice.assertEquals
    Lattice.derived
  }

  given bottom[A]: Bottom[SingleWriterWriteOnceMap[A]] = Bottom.derived

  def unchanged[A]: SingleWriterWriteOnceMap[A] = bottom.empty
}
