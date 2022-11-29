package de.tu_darmstadt.stg.daimpl
package causality

trait ForkEventJoinCausality[S: PartialOrdering]:
  val seed: S

  // TODO: add comparison
  extension (stamp: S)
    def fork: (S, S)
    def peek: S
    def event: Option[S]
    def join(otherStamp: S): Option[S]
    def sync(otherStamp: S): Option[(S, S)] = (stamp join otherStamp).map(_.fork)
    def send(otherStamp: S): Option[(S, S)] =
      for {
        newStamp <- stamp.event
      } yield (newStamp, newStamp.peek)
    def receive(otherStamp: S): Option[S] = for {
      joined <- stamp join otherStamp
      incremented <- joined.event
    } yield incremented
