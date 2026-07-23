package rdts.protocols.paxosVariants

import rdts.protocols.Paxos
import rdts.datatypes.Epoch
import rdts.protocols.Util.precondition
import rdts.protocols.Participants
import rdts.protocols.Util.Agreement
import rdts.base.Uid
import rdts.base.Bottom
import rdts.base.Lattice

case class ConfigurationRound[A](
    currentMembers: Set[Uid] = Set.empty[Uid],
    nextMembers: Paxos[Set[Uid]] = Paxos[Set[Uid]](),
    innerConsensus: Paxos[A] = Paxos[A]()
)

case class ReconfigurablePaxos[A](
    inner: Epoch[ConfigurationRound[A]] = Epoch(0, ConfigurationRound[A]())
):
    def nextDecision(using Participants) =
      precondition(
        inner.value.nextMembers.decision != Agreement.Undecided &&
        inner.value.innerConsensus.decision != Agreement.Undecided
      ) {
        ReconfigurablePaxos(Epoch(
          inner.counter + 1,
          ConfigurationRound(
            inner.value.nextMembers.result.get,
            Paxos(),
            Paxos()
          )
        ))
      }

object ReconfigurablePaxos:
    given [A]: Bottom[ReconfigurablePaxos[A]]  = Bottom.provide(ReconfigurablePaxos())
    given [A]: Lattice[ReconfigurablePaxos[A]] =
        given Lattice[ConfigurationRound[A]] = Lattice.derived
        Lattice.derived
