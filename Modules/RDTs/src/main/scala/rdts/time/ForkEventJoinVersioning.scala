package com.github.ckuessner
package causality

trait ForkEventJoinVersioning[S] {
  def seed: S
  val ordering: PartialOrdering[S]

  extension (stamp: S)
    def fork: (S, S)
    def peek: S
    def event: S
    def join(otherStamp: S): S
    def sync(otherStamp: S): (S, S) = (stamp join otherStamp).fork
}

object ForkEventJoinVersioning {
  def fromClock[S](using clock: ForkEventJoinClock[S]): ForkEventJoinVersioning[S] = new ForkEventJoinVersioning[S] {
    override def seed: S                      = clock.seed
    override val ordering: PartialOrdering[S] = clock.ordering

    extension (stamp: S)
      override def fork: (S, S)                = clock.fork(stamp)
      override def peek: S                     = clock.peek(stamp)
      override def event: S                    = clock.event(stamp)
      override def join(otherStamp: S): S      = clock.join(stamp)(otherStamp)
      override def sync(otherStamp: S): (S, S) = clock.sync(stamp)(otherStamp)
  }
}
