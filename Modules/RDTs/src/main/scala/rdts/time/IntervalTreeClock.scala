package rdts.time

import rdts.time.EventTree.given

case class IntervalTreeClock(idTree: IdTree, eventTree: EventTree):
    def fork: (IntervalTreeClock, IntervalTreeClock) =
        val (id1, id2) = idTree.split
        (IntervalTreeClock(id1, eventTree), IntervalTreeClock(id2, eventTree))

    @throws[IllegalArgumentException]("when the ids overlap")
    def join(otherStamp: IntervalTreeClock): IntervalTreeClock =
      IntervalTreeClock(
        idTree + otherStamp.idTree,
        eventTree `join` otherStamp.eventTree
      )

    /** Precondition: stamp is not anonymous
      *
      * @return
      * An IntervalTreeClock (i,e') such that e' = e + f * i
      */
    @throws[IllegalArgumentException]("when the id is anonymous")
    def event: IntervalTreeClock = {
      if idTree.isAnonymous then {
        throw IllegalArgumentException("Cannot perform events on an anonymous stamp")
      } else {
        copy(eventTree = eventTree.increment(idTree))
      }
    }

    def peek: IntervalTreeClock = IntervalTreeClock(IdTree.anonymous, eventTree)

    def sync(otherStamp: IntervalTreeClock): (IntervalTreeClock, IntervalTreeClock) =
      (this `join` otherStamp).fork

    def send: (IntervalTreeClock, IntervalTreeClock) =
        val newStamp = event
        (newStamp, newStamp.peek)

    def receive(otherStamp: IntervalTreeClock): IntervalTreeClock =
      (this `join` otherStamp).event

object IntervalTreeClock {
  given NormalForm[IntervalTreeClock] with
      extension (itc: IntervalTreeClock)
          def normalized: IntervalTreeClock =
              val idTreeNormalized    = itc.idTree.normalized
              val eventTreeNormalized = itc.eventTree.normalized
              if (idTreeNormalized eq itc.idTree) && (eventTreeNormalized eq itc.eventTree) then {
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
}
