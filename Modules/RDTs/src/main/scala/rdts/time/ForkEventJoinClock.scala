package com.github.ckuessner
package causality

trait ForkEventJoinClock[S]:
  def seed: S
  val ordering: PartialOrdering[S]

  extension (stamp: S)
    def fork: (S, S)
    def peek: S
    def event: S
    def join(otherStamp: S): S
    def sync(otherStamp: S): (S, S) = (stamp join otherStamp).fork
    def send: (S, S) =
      val newStamp = stamp.event
      (newStamp, newStamp.peek)
    def receive(otherStamp: S): S = (stamp join otherStamp).event
