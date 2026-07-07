package ex2026accessControl.evaluation.insecure

import crypto.{Hash, PublicIdentity}
import crypto.channels.PrivateIdentity
import replication.acl.bft.{Acl, BftDelta}
import replication.acl.sync.anti_entropy.{AclAntiEntropy, AntiEntropyCommunicator}

class NopAclAntiEntropy(
    id: PrivateIdentity,
    genesis: BftDelta[Acl],
    network: AntiEntropyCommunicator[?]
) extends AclAntiEntropy(id, genesis, _ => (), network) {

  override def currentAcl: (Set[Hash], Acl) = (Set.empty, Acl(Map.empty, Map.empty, Set.empty, Set.empty))

  override def receiveDeltas(deltas: Seq[BftDelta[Acl]], from: PublicIdentity): Unit        = ()
  override def respondToDeltaRequest(missing: Set[Hash], remote: PublicIdentity): Unit      = ()
  override def updatePeerAclKnowledge(remoteHeads: Set[Hash], remote: PublicIdentity): Unit = ()
  override def mutate(delta: Acl): Unit                                                     = ()

}
