package rdts.protocols.paxosVariants

import rdts.protocols.Util.Agreement
import rdts.protocols.Util.precondition
import rdts.datatypes.Epoch
import rdts.protocols.Paxos
import rdts.protocols.Participants
import rdts.base.Bottom
import rdts.base.Lattice

case class EpochPaxos[A](inner: Epoch[Paxos[A]] = Epoch(0, Paxos[A]())):
    def nextDecision(using Participants) =
      precondition(inner.value.decision != Agreement.Undecided) {
        EpochPaxos(Epoch(inner.counter + 1, Paxos()))
      }

object EpochPaxos:
    given [A]: Bottom[EpochPaxos[A]] = Bottom.provide(EpochPaxos[A]())

    given [A]: Lattice[EpochPaxos[A]] = Lattice.derived
