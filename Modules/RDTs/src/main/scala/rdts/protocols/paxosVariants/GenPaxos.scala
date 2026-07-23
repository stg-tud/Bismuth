package rdts.protocols.paxosVariants

import rdts.protocols.Paxos
import rdts.base.Uid
import rdts.protocols.Participants
import rdts.protocols.Util.precondition
import rdts.protocols.Util.Agreement
import rdts.base.Bottom
import rdts.base.Lattice

case class PaxosWithPredecessors[A](paxos: Paxos[A], predecessors: Set[Uid])
case class GenPaxos[A](operations: Map[Uid, PaxosWithPredecessors[A]] = Map.empty[Uid, PaxosWithPredecessors[A]]):
    def nextDecision(predecessors: Set[Uid])(using Participants) =
      precondition(predecessors.forall(p => operations(p).paxos.decision != Agreement.Undecided)) {
        GenPaxos(Map(Uid.gen() -> PaxosWithPredecessors(Paxos(), predecessors)))
      }

object GenPaxos:
    given [A]: Bottom[GenPaxos[A]]  = Bottom.provide(GenPaxos[A]())
    given [A]: Lattice[GenPaxos[A]] =
        given [A]: Lattice[PaxosWithPredecessors[A]] = Lattice.derived
        Lattice.derived
