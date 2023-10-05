package com.github.ckuessner
package causality

import causality.EventTree.given
import causality.IdTree.{*, given}
import causality.IntervalTreeClock.given

import scala.math

case class IntervalTreeClock(idTree: IdTree, eventTree: EventTree)

object IntervalTreeClock {
  given NormalForm[IntervalTreeClock] with
    extension (itc: IntervalTreeClock)
      def normalized: IntervalTreeClock =
        val idTreeNormalized    = itc.idTree.normalized
        val eventTreeNormalized = itc.eventTree.normalized
        if ((idTreeNormalized eq itc.idTree) && (eventTreeNormalized eq itc.eventTree)) {
          itc
        } else {
          IntervalTreeClock(idTreeNormalized, eventTreeNormalized)
        }

  given pOrd: PartialOrdering[IntervalTreeClock] with {
    override def lteq(x: IntervalTreeClock, y: IntervalTreeClock): Boolean =
      x.eventTree <= y.eventTree

    override def tryCompare(x: IntervalTreeClock, y: IntervalTreeClock): Option[Int] =
      summon[PartialOrdering[EventTree]].tryCompare(x.eventTree, y.eventTree)
  }

  given Clock: ForkEventJoinClock[IntervalTreeClock] with
    val seed: IntervalTreeClock = IntervalTreeClock(IdTree.seed, EventTree.seed)

    override val ordering: PartialOrdering[IntervalTreeClock] = pOrd

    extension (stamp: IntervalTreeClock)
      def fork: (IntervalTreeClock, IntervalTreeClock) =
        val (id1, id2) = stamp.idTree.split
        (IntervalTreeClock(id1, stamp.eventTree), IntervalTreeClock(id2, stamp.eventTree))

      @throws[IllegalArgumentException]("when the ids overlap")
      def join(otherStamp: IntervalTreeClock): IntervalTreeClock =
        IntervalTreeClock(
          stamp.idTree + otherStamp.idTree,
          stamp.eventTree join otherStamp.eventTree
        )

      /** Precondition: stamp is not anonymous
        *
        * @return
        *   An IntervalTreeClock (i,e') such that e' = e + f * i
        */
      @throws[IllegalArgumentException]("when the id is anonymous")
      def event: IntervalTreeClock = {
        if (stamp.idTree.isAnonymous) {
          throw IllegalArgumentException("Cannot perform events on an anonymous stamp")
        } else {
          stamp.copy(eventTree = stamp.eventTree.increment(stamp.idTree))
        }
      }

      def peek: IntervalTreeClock = IntervalTreeClock(IdTree.anonymous, stamp.eventTree)
}

trait NormalForm[T]:
  extension (tree: T) def normalized: T
