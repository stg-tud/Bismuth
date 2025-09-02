package ex2024travel.lofi_acl.sync.bft

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import crypto.{Ed25519Util, PublicIdentity}
import crypto.channels.PrivateIdentity
import ex2024travel.lofi_acl.sync.bft.BftAclOpGraph.{EncodedDelegation, Signature}
import ex2024travel.lofi_acl.sync.{Acl, RDTSync}
import rdts.base.{Bottom, Lattice, Uid}
import rdts.filters.{Filter, Operation, PermissionTree}
import rdts.time.{Dot, Dots}

import java.util.concurrent.atomic.AtomicReference
import scala.util.{Failure, Success}

class SyncWithBftMonotonicAcl[RDT](
    private val localIdentity: PrivateIdentity,
    aclRoot: EncodedDelegation,
    onDeltaReceive: RDT => Unit = (_: RDT) => {} // Consumes a delta
)(using
    lattice: Lattice[RDT],
    bottom: Bottom[RDT],
    rdtJsonCodec: JsonValueCodec[RDT],
    filter: Filter[RDT]
) extends RDTSync[RDT] {

  private val antiEntropy                                 = BftFilteringAntiEntropy[RDT](localIdentity, aclRoot, this)
  @volatile private var antiEntropyThread: Option[Thread] = None

  private val localPublicId = localIdentity.getPublic

  private val rdtReference: AtomicReference[(Dots, RDT)] = AtomicReference(Dots.empty -> Bottom[RDT].empty)
  private val lastLocalRdtDot: AtomicReference[Dot]      = AtomicReference(Dot(Uid(localPublicId.id), -1))

  override def currentState: RDT = rdtReference.get()._2

  override def currentAcl: Acl = currentBftAcl._2

  def currentBftAcl: (BftAclOpGraph, Acl) = localAcl.get()

  // Only change using grantPermissions!
  private val localAcl: AtomicReference[(BftAclOpGraph, Acl)] = {
    aclRoot.decode match
      case Failure(exception)         => throw exception
      case Success((sig, delegation)) =>
        val opGraph = BftAclOpGraph(sig, Map(sig -> delegation), Set(sig))
        AtomicReference((opGraph, opGraph.reconstruct(Set(sig)).get))
  }

  def grantPermissions(affectedUser: PublicIdentity, realm: PermissionTree, typeOfPermission: Operation): Unit = {
    val (read, write) = typeOfPermission match
      case rdts.filters.Operation.READ  => (realm, PermissionTree.empty)
      case rdts.filters.Operation.WRITE => (realm, realm)

    localAcl.synchronized {
      val old @ (opGraph, acl) = localAcl.get()
      val privateKey           = localIdentity.identityKey.getPrivate

      val (updatedOpGraph, aclDelta) = opGraph.delegateAccess(localPublicId, privateKey, affectedUser, read, write)
      val updatedAcl                 = acl.addPermissions(affectedUser, read, write)
      require(localAcl.compareAndSet(old, (updatedOpGraph, updatedAcl)))

      antiEntropy.broadcastAclDelegation(aclDelta)
    }
  }

  def applyAclIfPossible(encodedDelegation: EncodedDelegation): Set[Signature] = {
    localAcl.synchronized {
      val old @ (opGraph, acl) = localAcl.get()
      opGraph.receive(encodedDelegation.sig, encodedDelegation.op) match
        case Left(missingSignatures) => return missingSignatures
        case Right(updatedOpGraph)   =>
          val updatedAcl = updatedOpGraph.reconstruct(updatedOpGraph.heads).get
          assert {
            val delegation  = encodedDelegation.decode.get._2
            val expectedAcl = acl.addPermissions(delegation.delegatee, delegation.read, delegation.write)
            updatedAcl.read == expectedAcl.read && updatedAcl.write == expectedAcl.write
          }
          require(localAcl.compareAndSet(old, (updatedOpGraph, updatedAcl)))
          Set.empty
    }
  }

  def mutateState(deltaMutator: RDT => RDT): Unit = {
    val dot = lastLocalRdtDot.updateAndGet(dot => dot.advance)
    antiEntropy.mutateRdt(dot, deltaMutator(rdtReference.get()._2))
  }

  override def createInvitation: BftInvitation = {
    val connectionString = s"localhost:${antiEntropy.listenPort.getOrElse(-1)}"
    BftInvitation(aclRoot, Ed25519Util.generateNewKeyPair, localIdentity.getPublic, connectionString)
  }

  def connect(remoteUser: PublicIdentity, remoteAddress: String): Unit = {
    val hostParts = remoteAddress.split(":")
    require(hostParts.length == 2)
    antiEntropy.newPeers(Set(remoteUser -> (hostParts(0), hostParts(1).toInt)))
  }

  override def receivedDelta(dot: Dot, delta: RDT): Unit =
    val _ = rdtReference.updateAndGet { case (dots, rdt) => dots.add(dot) -> rdt.merge(delta) }
    onDeltaReceive(delta)

  def start(): Unit = {
    synchronized {
      require(antiEntropyThread.isEmpty)
      antiEntropyThread = Some(antiEntropy.start())
    }
  }

  def stop(): Unit = {
    synchronized {
      require(antiEntropyThread.nonEmpty)
      antiEntropy.stop()
      antiEntropyThread.get.interrupt()
      antiEntropyThread = None
    }
  }
}
