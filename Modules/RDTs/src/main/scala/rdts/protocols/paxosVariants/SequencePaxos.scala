package rdts.protocols.paxosVariants

import rdts.protocols.Paxos
import rdts.protocols.Util.precondition
import rdts.protocols.Participants
import rdts.protocols.Util.Agreement
import rdts.base.Lattice
import rdts.base.Bottom

case class SequencePaxos[A](log: List[Paxos[A]]):
    def nextDecision(using Participants) =
      precondition(log.forall(_.decision != Agreement.Undecided)) {
        SequencePaxos(log :+ Paxos())
      }

object SequencePaxos:
    given [A]: Bottom[SequencePaxos[A]]  = Bottom.provide(SequencePaxos[A](List.empty))
    given [A]: Lattice[SequencePaxos[A]] = Lattice.derived
