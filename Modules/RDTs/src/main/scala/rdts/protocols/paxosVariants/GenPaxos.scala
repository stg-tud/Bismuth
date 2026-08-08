package rdts.protocols.paxosVariants

import rdts.base.{Bottom, Lattice, Uid}
import rdts.protocols.Util.{Agreement, precondition}
import rdts.protocols.{Participants, Paxos}

case class PaxosWithPredecessors[A](paxos: Paxos[A], predecessors: Set[Uid])
case class GenPaxos[A](operations: Map[Uid, PaxosWithPredecessors[A]] = Map.empty[Uid, PaxosWithPredecessors[A]]):
    def nextDecision(predecessors: Set[Uid])(using Participants): GenPaxos[Nothing] =
      precondition(predecessors.forall(p => operations(p).paxos.decision != Agreement.Undecided)) {
        GenPaxos(Map(Uid.gen() -> PaxosWithPredecessors(Paxos(), predecessors)))
      }

object GenPaxos:
    given [A]: Bottom[GenPaxos[A]]  = Bottom.provide(GenPaxos[A]())
    given [A]: Lattice[GenPaxos[A]] =
        given [A]: Lattice[PaxosWithPredecessors[A]] = Lattice.derived
        Lattice.derived
