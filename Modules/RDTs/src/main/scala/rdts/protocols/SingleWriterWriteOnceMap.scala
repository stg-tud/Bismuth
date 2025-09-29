package rdts.protocols

import rdts.base.LocalUid.replicaId
import rdts.base.{Bottom, Lattice, LocalUid, Uid}

/** Assumes that all local modifications are totally ordered. */
case class SingleWriterWriteOnceMap[A](inner: Map[Uid, A]) {
  def write(v: A)(using LocalUid): SingleWriterWriteOnceMap[A] =
    if inner.contains(replicaId)
    then SingleWriterWriteOnceMap.unchanged
    else SingleWriterWriteOnceMap(Map(replicaId -> v))

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
