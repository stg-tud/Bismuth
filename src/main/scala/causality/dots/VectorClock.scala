package de.tu_darmstadt.stg.daimpl
package causality.dots

import causality.ForkEventJoinClock
import causality.dots.Defs.{Id, Time}
import util.MapHelper.max

import scala.math.PartialOrdering
import scala.util.Random

case class VectorClock(timestamps: Map[Id, Time] = Map()) {
  def merged(other: VectorClock): VectorClock = VectorClock(max(timestamps, other.timestamps))

  def merged(other: Map[Id, Time]): VectorClock = VectorClock(max(timestamps, other))

  def merged(other: Dot): VectorClock = merged(Map(other.replicaId -> other.time))

  def advance(replicaId: Id): VectorClock = VectorClock(
    timestamps = timestamps + (replicaId -> (timestamps.getOrElse(replicaId, 0L) + 1L))
  )

  def timeOf(replicaId: Id): Time = timestamps.getOrElse(replicaId, 0)

  def clockOf(replicaId: Id): Dot = Dot(replicaId, timeOf(replicaId))

  def contains(timestamp: Dot): Boolean = timestamps.getOrElse(timestamp.replicaId, 0L) >= timestamp.time
}

object VectorClock {
  given pOrd: PartialOrdering[VectorClock] with {
    override def tryCompare(x: VectorClock, y: VectorClock): Option[Int] = {
      if (x.timestamps.isEmpty) return Some(0)
      if (x.timestamps.keySet != y.timestamps.keySet) return None

      val clockPairs  = x.timestamps.keySet.map(key => (x.timestamps(key), y.timestamps(key)))
      val comparisons = clockPairs map { case (x, y) => x compare y }

      if (comparisons.max < 0) return Some(-1)
      if (comparisons.min > 0) return Some(1)
      if (comparisons.min == 0 && comparisons.max == 0) return Some(0)
      return None
    }

    override def lteq(x: VectorClock, y: VectorClock): Boolean = {
      if (x.timestamps.isEmpty) return true
      val anyXGreaterY = x.timestamps.exists { case (key, xTimeStampForKey) =>
        xTimeStampForKey > y.timestamps.getOrElse(key, 0L)
      }

      !anyXGreaterY
    }
  }

  given Clock: ForkEventJoinClock[(Id, VectorClock)] with {
    override def seed: (Id, VectorClock) = (Random.nextLong(), VectorClock())

    override val ordering: PartialOrdering[(Id, VectorClock)] = new PartialOrdering[(Id, VectorClock)]:
      override def tryCompare(x: (Id, VectorClock), y: (Id, VectorClock)): Option[Int] = pOrd.tryCompare(x._2,y._2)

      override def lteq(x: (Id, VectorClock), y: (Id, VectorClock)): Boolean = pOrd.lt(x._2, y._2)

    extension (localVectorClock: (Id, VectorClock))
      override def fork: ((Id, VectorClock), (Id, VectorClock)) = {
        (localVectorClock, (Random.nextLong(), localVectorClock._2))
      }

      override def join(remoteVectorClock: (Id, VectorClock)): (Id, VectorClock) = {
        (localVectorClock._1, localVectorClock._2.merged(remoteVectorClock._2))
      }

      override def event: (Id, VectorClock) = {
        (localVectorClock._1, localVectorClock._2.advance(localVectorClock._1))
      }

      override def peek: (Id, VectorClock) = {
        (0, localVectorClock._2)
      }

      override def sync(remoteVectorClock: (Id, VectorClock)): ((Id, VectorClock), (Id, VectorClock)) = {
        val mergedEvent = remoteVectorClock._2 merged remoteVectorClock._2
        ((localVectorClock._1, mergedEvent), (remoteVectorClock._1, mergedEvent))
      }
  }
}
