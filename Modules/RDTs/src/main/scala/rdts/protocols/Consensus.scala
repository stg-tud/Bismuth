package rdts.protocols

import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import Participants.participants

// Type class for consensus algorithms
trait Consensus[C[_]] {
  extension [A](c: C[A]) def propose(value: A)(using LocalUid, Participants): C[A]
  extension [A](c: C[A]) def result(using Participants): Option[A]
  extension [A](c: C[A]) def members(using Participants): Set[Uid] = participants
  extension [A](c: C[A]) def upkeep()(using LocalUid, Participants): C[A]

  def empty[A]: C[A]
  def lattice[A]: Lattice[C[A]]
}

object Consensus {
  given lattice[A, C[_]: Consensus]: Lattice[C[A]] = Consensus[C].lattice
  given bottom[A, C[_]: Consensus]: Bottom[C[A]] with
    override def empty: C[A] = Consensus[C].empty

  def apply[C[_]](using ev: Consensus[C]): Consensus[C] = ev
}
