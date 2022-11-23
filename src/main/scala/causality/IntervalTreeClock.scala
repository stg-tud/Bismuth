package de.tu_darmstadt.stg.daimpl
package causality

import EventTree.given
import IdTree.{*, given}
import IntervalTreeClock.given

import scala.math

case class IntervalTreeClock(idTree: IdTree, eventTree: EventTree)

object IntervalTreeClock {
  given NormalForm[IntervalTreeClock] with
    extension (itc: IntervalTreeClock)
      def normalize: IntervalTreeClock =
        IntervalTreeClock(itc.idTree.normalize, itc.eventTree.normalize)

  given PartialOrdering[IntervalTreeClock] with {
    override def lteq(x: IntervalTreeClock, y: IntervalTreeClock): Boolean =
      x.eventTree <= y.eventTree

    override def tryCompare(x: IntervalTreeClock, y: IntervalTreeClock): Option[Int] =
      summon[PartialOrdering[EventTree]].tryCompare(x.eventTree, y.eventTree)
  }

  given ForkEventJoinCausality[IntervalTreeClock] with
    val seed: IntervalTreeClock = IntervalTreeClock(IdTree.seed, EventTree.seed)

    extension (stamp: IntervalTreeClock)
      def fork: (IntervalTreeClock, IntervalTreeClock) =
        val (id1, id2) = stamp.idTree.split
        (IntervalTreeClock(id1, stamp.eventTree), IntervalTreeClock(id2, stamp.eventTree))

      def join(otherStamp: IntervalTreeClock): IntervalTreeClock =
        val newId = stamp.idTree + otherStamp.idTree
        val newEventTree = stamp.eventTree join otherStamp.eventTree
        IntervalTreeClock(newId, newEventTree)

      /**
       * Precondition: stamp is not anonymous (i.e., idTree is 0)
       *
       * @return An IntervalTreeClock (i,e') such that e' = e + f * i
       */
      def event: IntervalTreeClock = {
        if stamp.idTree == IdTree.anonymous then
          throw new IllegalArgumentException("Cannot perform events on an anonymous stamp")
        IntervalTreeClock(
          stamp.idTree,
          stamp.eventTree.increment(stamp.idTree)
        )
      }

      def peek: IntervalTreeClock = IntervalTreeClock(IdTree.anonymous, stamp.eventTree)
}

trait NormalForm[T]:
  extension (tree: T) def normalize: T


