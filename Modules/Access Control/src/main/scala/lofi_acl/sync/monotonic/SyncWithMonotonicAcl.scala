package lofi_acl.sync.monotonic

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import crypto.channels.PrivateIdentity
import crypto.{Ed25519Util, PublicIdentity}
import lofi_acl.sync.monotonic.MonotonicAclSyncMessage.*
import lofi_acl.sync.monotonic.SyncWithMonotonicAcl.messageJsonCodec
import lofi_acl.sync.{DeltaMapWithPrefix, Replica}
import rdts.base.{Bottom, Lattice, Uid}
import rdts.filters.{Filter, PermissionTree}
import rdts.time.{Dot, Dots}

import java.util.concurrent.atomic.AtomicReference

class SyncWithMonotonicAcl[RDT](
    private val localIdentity: PrivateIdentity,
    rootOfTrust: PublicIdentity,
    initialAclDeltas: List[AclDelta[RDT]],
    onDeltaReceive: RDT => Unit
)(using
    lattice: Lattice[RDT],
    bottom: Bottom[RDT],
    rdtJsonCode: JsonValueCodec[RDT],
    filter: Filter[RDT]
) extends Replica[RDT] {

  private val antiEntropy =
    FilteringAntiEntropy[RDT](localIdentity, rootOfTrust, initialAclDeltas, DeltaMapWithPrefix.empty, this)
  @volatile private var antiEntropyThread: Option[Thread] = None

  private val localPublicId = localIdentity.getPublic

  def currentState: RDT = rdtReference.get()._2

  private val rdtReference: AtomicReference[(Dots, RDT)] = AtomicReference((Dots.empty, bottom.empty))
  private val lastLocalRdtDot: AtomicReference[Dot]      = AtomicReference(Dot(Uid(localPublicId.id), -1))

  private val lastLocalAclDot: AtomicReference[Dot] = {
    val localId = localIdentity.getPublic.id
    AtomicReference(
      initialAclDeltas
        .filter(_.dot.place.delegate == localId)
        .maxByOption(_.dot.time)
        .map(_.dot)
        .getOrElse(Dot(Uid(localIdentity.getPublic.id), -1))
    )
  }

  def currentAcl: MonotonicAcl[RDT] = antiEntropy.acl

  def grantPermissions(affectedUser: PublicIdentity, read: PermissionTree, write: PermissionTree): Unit = {
    val dot = lastLocalAclDot.updateAndGet(dot => dot.advance)
    antiEntropy.grantPermission(dot, affectedUser, read, write)
  }

  def mutateState(deltaMutator: RDT => RDT): Unit = {
    val dot = lastLocalRdtDot.updateAndGet(dot => dot.advance)
    antiEntropy.mutateRdt(dot, deltaMutator(rdtReference.get()._2))
  }

  override def createInvitation: MonotonicInvitation = {
    val connectionString = s"localhost:${antiEntropy.listenPort.getOrElse(-1)}"
    MonotonicInvitation(rootOfTrust, Ed25519Util.generateNewKeyPair, localIdentity.getPublic, connectionString)
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

object SyncWithMonotonicAcl {
  import lofi_acl.sync.JsoniterCodecs.given
  given signatureCodec: JsonValueCodec[Signature | Null] = new JsonValueCodec[Signature | Null]:
      override def decodeValue(in: JsonReader, default: Signature | Null): Signature | Null =
          val sigArray = in.readBase64AsBytes(Array.empty)
          if sigArray.isEmpty then null
          else Signature(sigArray)
      override def encodeValue(sig: Signature | Null, out: JsonWriter): Unit =
        if sig == null then out.writeVal("")
        else out.writeBase64Val(sig.sig, true)
      override def nullValue: Signature | Null = null

  given messageJsonCodec[RDT: JsonValueCodec]: JsonValueCodec[MonotonicAclSyncMessage[RDT]] = JsonCodecMaker.make(
    CodecMakerConfig.withMapAsArray(true)
  )
}
