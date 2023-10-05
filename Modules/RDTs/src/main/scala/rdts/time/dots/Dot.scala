package com.github.ckuessner
package causality.dots

import causality.dots.Defs.{Id, Time}

// Lamport clock with replicaId
case class Dot(replicaId: Id, time: Time) {
  def advance(replicaId: Id): Dot = Dot(replicaId, time + 1)
}

object Dot {
  given dotPartialOrdering: PartialOrdering[Dot] with {
    override inline def tryCompare(x: Dot, y: Dot): Option[Int] = {
      if (x.replicaId == y.replicaId) {
        if (x.time < y.time) Some(-1)
        else if (x.time > y.time) Some(1)
        else Some(0)
      } else None
    }

    override inline def lteq(x: Dot, y: Dot): Boolean = {
      x.replicaId == y.replicaId && x.time <= y.time
    }
  }
}
