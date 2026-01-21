package lofi_acl.sync.signed

import channels.MessageBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import crypto.PublicIdentity
import crypto.channels.PrivateIdentity
import lofi_acl.bft.*
import lofi_acl.bft.AclRdt.given_Encoder_SignedDelta
import lofi_acl.bft.HashDag.Hashable
import lofi_acl.sync.signed.FilteredAntiEntropy.SyncMsg
import lofi_acl.sync.signed.FilteredAntiEntropy.SyncMsg.PeerGossip
import lofi_acl.sync.{ChannelConnectionManager, ConnectionManager, MessageReceiver}
import rdts.base.Bottom
import rdts.time.{Dot, Dots}

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicReference

class FilteredAntiEntropy[RDT: {JsonValueCodec, Bottom}](
    localIdentity: PrivateIdentity,
    connectionManagerProvider: (PrivateIdentity, MessageReceiver[MessageBuffer]) => ConnectionManager =
      (id, receiver) => ChannelConnectionManager(id.tlsKeyPem, id.tlsCertPem, id.getPublic, receiver),
    initialAclHashDag: HashDag[SignedDelta[Acl], Acl]
) extends MessageReceiver[SyncMsg[RDT]] {
  val msgQueue: LinkedBlockingQueue[(SyncMsg[RDT], PublicIdentity)] = LinkedBlockingQueue()
  val comm: Communication[SyncMsg[RDT]]                             = ???
  val aclAntiEntropy                                                = AclAntiEntropy(localIdentity, initialAclHashDag)

  override def receivedMessage(msg: SyncMsg[RDT], fromUser: PublicIdentity): Unit = msg match {
    case SyncMsg.DataDelta(delta)                        => ???
    case SyncMsg.AclDeltas(deltas)                       => aclAntiEntropy.receiveDeltas(deltas, fromUser)
    case SyncMsg.PeerGossip(peers)                       => ???
    case SyncMsg.DotGossip(dataDeltas, aclHeads)         => ???
    case SyncMsg.PleaseSendMe(dataDeltas, aclDeltasDots) => ???
  }
}

trait Communication[Msg: JsonValueCodec] {
  def send(to: PublicIdentity, msg: Msg): Unit
  def sendMultiple(to: PublicIdentity, msg: Msg): Unit
  def handlePeerGossip(peers: Set[(PublicIdentity, (String, Int))]): Unit
}

class AclAntiEntropy[RDT](private val id: PrivateIdentity, initialHashDag: HashDag[SignedDelta[Acl], Acl]) {

  private val aclRdt = AclRdt(id, cachedAclLookup)

  @volatile private var hashDag: HashDag[SignedDelta[Acl], Acl] = initialHashDag
  private val latestAcl                                         = AtomicReference((Set.empty[Hash], Bottom[Acl].empty))

  @volatile private var deltasInBacklog                           = Set.empty[Hash]
  private val knownMissingDeltas: AtomicReference[Set[Hash]]      = AtomicReference(Set.empty)
  @volatile private var backlog: Vector[(Hash, SignedDelta[Acl])] = Vector.empty

  private def cachedAclLookup(heads: Set[Hash]): Option[Acl] = {
    val (latestHeads, acl) = latestAcl.get()
    if latestHeads == heads then Some(acl) else None
  }

  def receiveDeltas(deltas: Vector[SignedDelta[Acl]], from: PublicIdentity): Unit = {
    synchronized {
      var changed    = true
      val hashes     = deltas.map(Hashable[SignedDelta[Acl]].hash(_))
      var toApply    = hashes.zip(deltas).concat(backlog)
      var newMissing = Set.empty[Hash]
      // TODO: A more efficient/robust approach would be to topologically sort the deltas
      while changed do {
        changed = false
        toApply = toApply.filter { (hash, delta) =>
          try {
            aclRdt.receiveWithComputedHash(hash, delta, hashDag) match {
              case Left(missingForDelta) =>
                newMissing = newMissing union missingForDelta
                true // Keep delta, not yet applied
              case Right(updatedHashDag) =>
                changed = true;
                hashDag = updatedHashDag
                latestAcl.updateAndGet((_, acl) => (hashDag.heads, acl.merge(delta.state)))
                false // Remove applied delta from queue
            }
          } catch {
            case e: IllegalArgumentException =>
              System.err.println(s"Illegal delta: $delta")
              false // Remove illegal delta
          }
        }
      }

      backlog = toApply
      deltasInBacklog = backlog.map(_._1).toSet
      knownMissingDeltas.updateAndGet(oldMissing => (oldMissing ++ newMissing) diff deltasInBacklog): Unit

      // TODO: Notify about changed ACL?
      // TODO: Notify about newly missing deltas?
    }
  }

  def updatePeerAclKnowledge(remoteHeads: Set[Acl], peer: PublicIdentity): Unit = {
    // Check if equal
    // -> Is less on remote?
    // -> Is advanced on remote?
  }
}

object FilteredAntiEntropy {
  enum SyncMsg[RDT]:
      case DataDelta(delta: FilterableSignedDelta[RDT])
      case AclDeltas(delta: Vector[SignedDelta[Acl]])
      case PeerGossip(peers: Set[(PublicIdentity, (String, Int))])
      case DotGossip(dataDeltas: Dots, aclHeads: Set[Hash])
      case PleaseSendMe(dataDeltas: Dots, aclDeltasDots: Set[Hash])

  given msgCodec[RDT: JsonValueCodec]: JsonValueCodec[SyncMsg[RDT]] = {
    import lofi_acl.sync.JsoniterCodecs.given
    import replication.JsoniterCodecs.given
    given JsonValueCodec[FilterableSignedDelta[RDT]] = JsonCodecMaker.make
    JsonCodecMaker.make
  }
}
