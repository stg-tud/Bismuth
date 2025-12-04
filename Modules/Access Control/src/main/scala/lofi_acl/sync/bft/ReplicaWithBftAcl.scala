package lofi_acl.sync.bft

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import crypto.channels.PrivateIdentity
import crypto.{Ed25519Util, PublicIdentity}
import BftAclOpGraph.Signature
import lofi_acl.sync.Replica
import rdts.base.{Bottom, Lattice, Uid}
import rdts.filters.{Filter, PermissionTree}
import rdts.time.{Dot, Dots}

import java.util.concurrent.atomic.AtomicReference
import scala.util.{Failure, Success}

class ReplicaWithBftAcl[RDT](using
    lattice: Lattice[RDT],
    bottom: Bottom[RDT],
    rdtJsonCodec: JsonValueCodec[RDT],
    filter: Filter[RDT]
)(
    private val localIdentity: PrivateIdentity,
    aclRoot: SerializedAclOp,
    onDeltaReceive: RDT => Unit = (_: RDT) => {}, // Consumes a delta
    antiEntropyProvider: (PrivateIdentity, SerializedAclOp, ReplicaWithBftAcl[RDT]) => BftFilteringAntiEntropy[RDT] =
      (localIdentity, aclRoot, sync: ReplicaWithBftAcl[RDT]) =>
        BftFilteringAntiEntropy[RDT](localIdentity, aclRoot, sync)
) extends Replica[RDT] {

  private val antiEntropy = antiEntropyProvider(localIdentity, aclRoot, this)

  private val localPublicId = localIdentity.getPublic

  private val rdtReference: AtomicReference[(Dots, RDT)] = AtomicReference(Dots.empty -> Bottom[RDT].empty)
  private val lastLocalRdtDot: AtomicReference[Dot]      = AtomicReference(Dot(Uid(localPublicId.id), -1))

  override def currentState: RDT = rdtReference.get()._2

  def currentAcl: BftAcl = currentOpGraph.latestAcl

  def currentOpGraph: BftAclOpGraph = localAcl.get()

  // Only change with lock on localAcl
  private val localAcl: AtomicReference[BftAclOpGraph] = {
    aclRoot.deserialize match
        case Failure(exception)    => throw exception
        case Success((sig, aclOp)) => AtomicReference(BftAclOpGraph.fromRootAclOp(sig, aclOp))
  }

  def grantPermissions(
      affectedUser: PublicIdentity,
      read: PermissionTree,
      write: PermissionTree
  ): Unit = {
    val privateKey                = localIdentity.identityKey.getPrivate
    var aclDelta: SerializedAclOp = null
    localAcl.synchronized {
      val oldOpGraph = localAcl.get()
      oldOpGraph.delegateAccess(localPublicId, privateKey, affectedUser, read, write) match {
        case (updatedOpGraph, op) =>
          aclDelta = op
          require(localAcl.compareAndSet(oldOpGraph, updatedOpGraph))
      }
    }

    antiEntropy.broadcastAclDelegation(aclDelta)
  }

  /** Applies aclOp if dependencies are met and op is legal. Returns the missing dependencies (or Set.empty). */
  def applyAclOpIfPossible(serializedAclOp: SerializedAclOp): Set[Signature] = {
    localAcl.synchronized {
      val oldOpGraph = localAcl.get()
      oldOpGraph.receive(serializedAclOp) match
          case Left(missingSignatures) => return missingSignatures
          case Right(updatedOpGraph)   =>
            if !localAcl.compareAndSet(oldOpGraph, updatedOpGraph)
            then // Sanity check, the lock should prevent this
                throw IllegalStateException("Could not apply update to ACL reference")

            updatedOpGraph.ops(serializedAclOp.signatureAsString) match {
              case RemovalOp(_, removed, _) => antiEntropy.removePeer(removed)
              case _                        => ()
            }

            Set.empty
    }
  }

  def mutateState(deltaMutator: RDT => RDT): Unit = {
    val dot = lastLocalRdtDot.updateAndGet(dot => dot.advance)
    antiEntropy.mutateRdt(dot, deltaMutator(rdtReference.get()._2))
  }

  def address: String = s"localhost:${antiEntropy.listenPort.getOrElse(-1)}"

  override def createInvitation: BftInvitation =
    BftInvitation(aclRoot, Ed25519Util.generateNewKeyPair, localIdentity.getPublic, address)

  def connect(remoteUser: PublicIdentity, remoteAddress: String): Unit = {
    val hostParts = remoteAddress.split(":")
    require(hostParts.length == 2)
    antiEntropy.connect(remoteUser, hostParts(0), hostParts(1).toInt)
  }

  override def receivedDelta(dot: Dot, delta: RDT): Unit =
      val _ = rdtReference.updateAndGet { case (dots, rdt) => dots.add(dot) -> rdt.merge(delta) }
      onDeltaReceive(delta)

  def start(): Unit =
    antiEntropy.start()

  def stop(): Unit =
    antiEntropy.stop()
}
