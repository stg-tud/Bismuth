package rdts.datatypes

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

  def observed: Dots = set.union(unset)
}

object EnableWinsFlag {

  given lattice: Lattice[EnableWinsFlag] = Lattice.derived

  given decompose: Decompose[EnableWinsFlag] = Decompose.atomic

  val empty: EnableWinsFlag = EnableWinsFlag(Dots.empty, Dots.empty)

  /** Subsumption checks the set and unset separately if they subsume their counterpart in the buffered delta. A
    * remove makes a previous enable modification redundant. If the dot for enabling the flag is only contained in unset
    * and not in set, it is equivalent to only being present in unset. An enable modification does not make any previous
    * delta redundant other than the delta containing the exact same dot for enabling the flag.
    */
  given historized: Historized[EnableWinsFlag] = (delta, bufferedDelta) => delta.unset.contains(bufferedDelta.observed)

}
