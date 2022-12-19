package de.tu_darmstadt.stg.daimpl
package causality

trait ForkEventJoinCausality[S: PartialOrdering]:
  val seed: S

  // TODO: add comparison
  extension (stamp: S)
    def fork: (S, S)
    def peek: S
    def event: S
    def join(otherStamp: S): S
    def sync(otherStamp: S): (S, S) = (stamp join otherStamp).fork
    def send(otherStamp: S): (S, S) =
      val newStamp = stamp.event
      (newStamp, newStamp.peek)
    def receive(otherStamp: S): S = (stamp join otherStamp).event
