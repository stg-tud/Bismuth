package de.tu_darmstadt.stg.daimpl
package causality.dots

import causality.dots.Defs.{Id, Time}

import scala.math.Ordered.orderingToOrdered

// Lamport clock with replicaId
case class Dot(time: Time, replicaId: Id) extends Ordered[Dot] {
  override def compare(that: Dot): Int = (time, replicaId) compare (that.time, that.replicaId)

  def advance(replicaId: String): Dot = Dot(time + 1, replicaId)
}

object Dot {
  def max(a: Dot, b: Dot): Dot = {
    if (a >= b) a
    else b
  }
}
