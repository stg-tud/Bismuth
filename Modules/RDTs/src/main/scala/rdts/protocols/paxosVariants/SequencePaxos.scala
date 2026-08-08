package rdts.protocols.paxosVariants

import rdts.base.{Bottom, Lattice}
import rdts.protocols.Util.{Agreement, precondition}
import rdts.protocols.{Participants, Paxos}

case class SequencePaxos[A](log: List[Paxos[A]]):
    def nextDecision(using Participants): SequencePaxos[A] =
      precondition(log.forall(_.decision != Agreement.Undecided)) {
        SequencePaxos(log :+ Paxos())
      }

object SequencePaxos:
    given [A]: Bottom[SequencePaxos[A]]  = Bottom.provide(SequencePaxos[A](List.empty))
    given [A]: Lattice[SequencePaxos[A]] = Lattice.derived
