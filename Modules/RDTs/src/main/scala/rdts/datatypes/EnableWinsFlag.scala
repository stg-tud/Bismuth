package rdts.datatypes

import rdts.base.Historized.MetaDelta
import rdts.base.{Bottom, Decompose, Historized, Lattice, LocalUid}
import rdts.time.Dots

/** An EWFlag (Enable-Wins Flag) is a Delta CRDT modeling a boolean flag.
  *
  * When the flag is concurrently disabled and enabled then the enable operation wins, i.e. the resulting flag is enabled.
  */
case class EnableWinsFlag(set: Dots, unset: Dots) derives Bottom {

  def read: Boolean = !unset.contains(set)

  def enable(using LocalUid)(): EnableWinsFlag = {
    val nextDot = set.nextDot(LocalUid.replicaId)
    EnableWinsFlag(Dots.single(nextDot), Dots.empty)
  }

  def disable(): EnableWinsFlag = EnableWinsFlag(Dots.empty, set)
}

object EnableWinsFlag {

  given lattice: Lattice[EnableWinsFlag] = Lattice.derived

  given decompose: Decompose[EnableWinsFlag] = Decompose.atomic

  val empty: EnableWinsFlag = EnableWinsFlag(Dots.empty, Dots.empty)

  given historized: Historized[EnableWinsFlag] = (delta: EnableWinsFlag, buffer: Iterable[MetaDelta[EnableWinsFlag]]) =>
    // TODO: erklären warum das funktioniert,
    // delta.unset `subsumes` bufferedDelta.delta.unset
    buffer.filter(bufferedDelta => delta.unset.contains(bufferedDelta.delta.set)).getAllDots

}
