package rdts.time

import rdts.base.Lattice

import scala.annotation.tailrec

/** An operation (or batch of operations) together with the dots of everything that was applied before it was created. */
final case class Causal[+A](predecessors: Dots, value: A)

object Causal {

  /** Merging unions the predecessors, so a merged value is ready once the predecessors of all parts are applied
    * (except for the parts themselves, see [[CausalBuffer]]). Requires a lattice, because a merged value may be
    * delivered to a receiver that already applied some of its parts.
    */
  given lattice[A: Lattice]: Lattice[Causal[A]] = Lattice.derived
}

/** Receiver-side causal delivery for operation-based data types.
  *
  * The dissemination layer (e.g., a broadcast) assigns dots to deltas and delivers each delta at least once in any order.
  * The buffer holds back deltas until all of their predecessors have been applied, and drops duplicates.
  *
  * Usage:
  *  - local: apply the operation, broadcast `Causal(buffer.applied, op)`, then [[markApplied]] the assigned dot.
  *  - remote: [[receive]] the delivered dots and value, and apply the returned ready values in order.
  */
final case class CausalBuffer[A](applied: Dots = Dots.empty, pending: List[(Dots, Causal[A])] = Nil) {

  /** Record locally applied dots. May release pending deltas that were waiting for these dots. */
  def markApplied(dots: Dots): (CausalBuffer[A], List[A]) =
    copy(applied = applied.union(dots)).drain(Nil)

  /** Buffer a delivered delta identified by `dots`. Returns all values that became ready, in causal order. */
  def receive(dots: Dots, delta: Causal[A]): (CausalBuffer[A], List[A]) =
    if dots <= applied || pending.exists((pendingDots, _) => dots <= pendingDots) then (this, Nil)
    else copy(pending = pending :+ (dots, delta)).drain(Nil)

  /** Dots that have been delivered, but are waiting for predecessors. */
  def waiting: Dots = pending.iterator.map(_._1).foldLeft(Dots.empty)(_ `union` _)

  // predecessors may include the delta’s own dots, e.g., if the dissemination layer merged deltas
  private def isReady(dots: Dots, delta: Causal[A]): Boolean =
    delta.predecessors.subtract(dots) <= applied

  @tailrec
  private def drain(acc: List[A]): (CausalBuffer[A], List[A]) =
    pending.find(isReady) match
        case None                        => (this, acc.reverse)
        case Some(ready @ (dots, delta)) =>
          copy(applied = applied.union(dots), pending = pending.filterNot(_ eq ready)).drain(delta.value :: acc)
}
