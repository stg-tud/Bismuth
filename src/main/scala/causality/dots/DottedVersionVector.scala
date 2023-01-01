package de.tu_darmstadt.stg.daimpl
package causality.dots

import causality.dots.Defs.{Id, Time}
import causality.dots.Dot
import causality.dots.impl.ArrayRanges
import lattices.SemiLattice

case class DottedVersionVector(internal: Map[Id, ArrayRanges]) {

  def rangeAt(replicaId: Id): ArrayRanges = internal.getOrElse(replicaId, ArrayRanges.empty)

  def clockOf(replicaId: Id): Option[Dot] = max(replicaId)

  def add(replicaId: Id, time: Time): DottedVersionVector =
    DottedVersionVector(
      internal.updated(
        replicaId,
        rangeAt(replicaId).add(time)
      )
    )

  def nextTime(replicaId: Id): Time = rangeAt(replicaId).next.getOrElse(0)

  def nextDot(replicaId: Id): Dot = Dot(nextTime(replicaId), replicaId)

  def diff(extern: DottedVersionVector): DottedVersionVector =
    DottedVersionVector(
      internal.map { case (id, range) =>
        val filtered = extern.internal.get(id).map { erange =>
          val keep = range.iterator.filterNot(erange.contains)
          ArrayRanges.from(keep)
        }
        id -> filtered.getOrElse(range)
      }
    )

  def intersect(other: DottedVersionVector): DottedVersionVector =
    DottedVersionVector {
      internal.flatMap { case (id, ranges) =>
        other.internal.get(id) match {
          case Some(otherRanges) =>
            val intersection = ranges intersect otherRanges
            if (intersection.isEmpty) None
            else Some(id -> intersection)
          case None => None
        }
      }
    }

  def union(other: DottedVersionVector): DottedVersionVector = DottedVersionVector.contextLattice.merged(this, other)

  def subtract(other: DottedVersionVector): DottedVersionVector = {
    DottedVersionVector(
      internal
        .map { case left @ (id, leftRanges) =>
          other.internal.get(id) match {
            case Some(rightRanges) => id -> (leftRanges subtract rightRanges)
            case None              => left
          }
        }
        .filterNot(_._2.isEmpty)
    )
  }

  def contains(d: Dot): Boolean = internal.get(d.replicaId).exists(_.contains(d.time))

  def toSet: Set[Dot] =
    internal.flatMap { case (key, tree) => tree.iterator.map(time => Dot(time, key)) }.toSet

  def max(replicaID: Id): Option[Dot] =
    internal.get(replicaID).flatMap(_.next.map(c => Dot(c - 1, replicaID)))

  def decompose(exclude: Dot => Boolean): Iterable[DottedVersionVector] =
    internal.flatMap { case (id, tree) =>
      tree.iterator.map(time => Dot(time, id)).filterNot(exclude).map(DottedVersionVector.one)
    }
  def forall(cond: Dot => Boolean): Boolean = internal.forall { case (id, tree) =>
    tree.iterator.forall(time => cond(Dot(time, id)))
  }

  def <=(other: DottedVersionVector): Boolean = internal.forall { case (id, leftRange) =>
    leftRange <= other.rangeAt(id)
  }
}

object DottedVersionVector {
  def single(replicaId: Id, time: Time): DottedVersionVector = empty.add(replicaId, time)

  def single(lamportClock: Dot): DottedVersionVector = empty.add(lamportClock.replicaId, lamportClock.time)

  val empty: DottedVersionVector = DottedVersionVector(Map.empty)

  def one(dot: Dot): DottedVersionVector = empty.add(dot.replicaId, dot.time)

  given contextLattice: SemiLattice[DottedVersionVector] = (left: DottedVersionVector, right: DottedVersionVector) => {
    DottedVersionVector(SemiLattice.merged(left.internal, right.internal))
  }

  def fromSet(dots: Set[Dot]): DottedVersionVector = DottedVersionVector(
    dots.groupBy(_.replicaId).map { case (key, times) =>
      key -> ArrayRanges.from(times.iterator.map(_.time))
    }
  )

}
