package com.github.ckuessner
package causality.dots

import causality.dots.Defs.{Id, Time}
import causality.dots.impl.ArrayRanges
import lattices.SemiLattice

import scala.annotation.targetName

case class DotSet(internal: Map[Id, ArrayRanges]) {

  def rangeAt(replicaId: Id): ArrayRanges = internal.getOrElse(replicaId, ArrayRanges.empty)

  def clockOf(replicaId: Id): Option[Dot] = max(replicaId)

  def add(replicaId: Id, time: Time): DotSet =
    DotSet(
      internal.updated(
        replicaId,
        rangeAt(replicaId).add(time)
      )
    )

  def nextTime(replicaId: Id): Time = rangeAt(replicaId).next.getOrElse(0)

  def nextDot(replicaId: Id): Dot = Dot(replicaId, nextTime(replicaId))

  def diff(extern: DotSet): DotSet =
    DotSet(
      internal.map { case (id, range) =>
        val filtered = extern.internal.get(id).map { erange =>
          val keep = range.iterator.filterNot(erange.contains)
          ArrayRanges.from(keep)
        }
        id -> filtered.getOrElse(range)
      }
    )

  def intersect(other: DotSet): DotSet =
    DotSet {
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

  def union(other: DotSet): DotSet = DotSet.contextLattice.merged(this, other)

  def subtract(other: DotSet): DotSet = {
    DotSet(
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
    internal.flatMap { case (id, arrayRanges) => arrayRanges.iterator.map(time => Dot(id, time)) }.toSet

  def max(replicaID: Id): Option[Dot] =
    internal.get(replicaID).flatMap(_.next.map(c => Dot(replicaID, c - 1)))

  def decompose(exclude: Dot => Boolean): Iterable[DotSet] =
    internal.flatMap { case (id, tree) =>
      tree.iterator.map(time => Dot(id, time)).filterNot(exclude).map(DotSet.one)
    }

  def forall(cond: Dot => Boolean): Boolean = internal.forall { case (id, tree) =>
    tree.iterator.forall(time => cond(Dot(id, time)))
  }

  @targetName("lteq")
  def <=(other: DotSet): Boolean = internal.forall { case (id, leftRange) =>
    leftRange <= other.rangeAt(id)
  }
}

object DotSet {
  def single(replicaId: Id, time: Time): DotSet = empty.add(replicaId, time)

  def single(dot: Dot): DotSet = empty.add(dot.replicaId, dot.time)

  val empty: DotSet = DotSet(Map.empty)

  def one(dot: Dot): DotSet = empty.add(dot.replicaId, dot.time)

  given contextLattice: SemiLattice[DotSet] = (left: DotSet, right: DotSet) => {
    DotSet(SemiLattice.merged(left.internal, right.internal))
  }

  def fromSet(dots: Set[Dot]): DotSet = DotSet(
    dots.groupBy(_.replicaId).map { case (key, times) =>
      key -> ArrayRanges.from(times.iterator.map(_.time))
    }
  )

}
