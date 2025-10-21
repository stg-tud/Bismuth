package rdts.time

import rdts.base.Uid

/** A Dot is a globally unique point in time.
  * Dots are partially ordered by their time per replicaId.
  */
case class Dot(place: Uid, time: Time) {
  def advance: Dot = Dot(place, time + 1)
  def dots: Dots   = Dots.single(this)

  override def equals(obj: Any): Boolean = obj match {
    case Dot(p, t) => p == place && t == time
    case _         => super.equals(obj)
  }
}

object Dot {

  def zero: Dot = Dot(Uid.zero, 0)

  /** While this seems plausible, it might, in general, be better to treat all dots as incomparable, we assume them to increase monotonically, but that is for optimization purposes, not because we use it anywhere else */
  @deprecated("probably not a good idea")
  def partialOrdering: PartialOrdering[Dot] = new:
    override def tryCompare(x: Dot, y: Dot): Option[Int] =
      if x.place == y.place
      then Some(Ordering[Long].compare(x.time, y.time))
      else None
    override def lteq(x: Dot, y: Dot): Boolean =
      x.place == y.place && x.time <= y.time

}
