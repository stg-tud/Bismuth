package lofi_acl.evaluation.insecure

import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.HashDag.Encoder
import lofi_acl.bft.{Acl, Hash}
import lofi_acl.sync.anti_entropy.{AclAntiEntropy, AntiEntropyCommunicator, FilteredRdtAntiEntropy, SignedDelta}
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.filters.Filter
import rdts.time.{Dot, Dots}

import scala.annotation.unused

class NonEnforcingNonSigningRdtAntiEntropy[State: {Decompose, Lattice, Bottom, Filter}](
    localIdentity: PrivateIdentity,
    onRdtChange: (Dots, State) => Unit,
    network: AntiEntropyCommunicator[State],
    aclAntiEntropy: AclAntiEntropy,
)(using Encoder[SignedDelta[State]])
    extends FilteredRdtAntiEntropy[State](localIdentity, onRdtChange, network, aclAntiEntropy) {

  override def onAclChanged(delta: Acl): Unit = ()

  override protected def sendDeltasFiltered(deltas: Iterable[SignedDelta[State]], remote: PublicIdentity): Unit =
    // Doesn't filter, ignores ACL
    network.sendDeltas(deltas.toSeq, Dots.empty, Set.empty, remote)

  override def receiveDeltas(
      unverifiedDeltas: Seq[SignedDelta[State]],
      @unused filtered: Dots,
      @unused remoteAcl: Set[Hash],
      remote: PublicIdentity
  ): Unit =
    // Ignores filtered and remoteAcl metadata, doesn't filter and doesn't verify signatures
    applyVerifiedDeltas(remote, unverifiedDeltas)

  override def localMutation(mutator: State => State): Unit =
      // Doesn't filter, doesn't sign
      val delta            = mutator(currentState._2)
      val decomposedDeltas = delta.decomposed
        .map(decomposedDelta => SignedDelta(Dot(localUid, dotCounter.getAndIncrement()), null, decomposedDelta))
        .toSeq

      if decomposedDeltas.nonEmpty then {
        applyVerifiedDeltas(localIdentity.getPublic, decomposedDeltas)
        broadcastDeltasFiltered(decomposedDeltas)
      }
}
