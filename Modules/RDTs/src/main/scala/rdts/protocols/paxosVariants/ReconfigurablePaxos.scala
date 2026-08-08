package rdts.protocols.paxosVariants

import rdts.base.{Bottom, Lattice, Uid}
import rdts.datatypes.Epoch
import rdts.protocols.Util.{Agreement, precondition}
import rdts.protocols.{Participants, Paxos}

case class ConfigurationRound[A](
    currentMembers: Set[Uid] = Set.empty[Uid],
    nextMembers: Paxos[Set[Uid]] = Paxos[Set[Uid]](),
    innerConsensus: Paxos[A] = Paxos[A]()
)

case class ReconfigurablePaxos[A](
    inner: Epoch[ConfigurationRound[A]] = Epoch(0, ConfigurationRound[A]())
):
    def nextDecision(using Participants): ReconfigurablePaxos[Nothing] =
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
