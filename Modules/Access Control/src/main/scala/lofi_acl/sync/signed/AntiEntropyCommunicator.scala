package lofi_acl.sync.signed

import channels.ArrayMessageBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import crypto.PublicIdentity
import lofi_acl.bft.{Acl, BftDelta, Hash}
import lofi_acl.sync.ConnectionManager
import lofi_acl.sync.signed.AclEnforcingSync.SyncMsg
import lofi_acl.sync.signed.AclEnforcingSync.SyncMsg.*
import rdts.time.Dots

trait AntiEntropyCommunicator[State] {
  def sendDeltas(
      deltas: Seq[SignedDelta[State]],
      filteredDeltas: Dots,
      aclVersion: Set[Hash],
      remote: PublicIdentity
  ): Unit

  def sendDeltas(deltas: Seq[BftDelta[Acl]], remote: PublicIdentity): Unit

  def requestDeltas(deltas: Dots, remote: PublicIdentity): Unit

  def requestDeltas(hashes: Set[Hash], remote: PublicIdentity): Unit

  def tellAclVersion(heads: Set[Hash], remote: PublicIdentity): Unit

  def tellRdtVersion(dots: Dots, remote: PublicIdentity): Unit

  def connectedPeers: Set[PublicIdentity]
}

class ConnectionManagerCommunicator[State](private val conn: ConnectionManager)(using JsonValueCodec[SyncMsg[State]])
    extends AntiEntropyCommunicator[State] {

  private inline def send(remote: PublicIdentity, syncMsg: SyncMsg[State]): Unit = {
    val buffer = ArrayMessageBuffer(writeToArray(syncMsg))
    conn.send(remote, buffer)
  }

  override def sendDeltas(
      deltas: Seq[SignedDelta[State]],
      filteredDeltas: Dots,
      aclVersion: Set[Hash],
      remote: PublicIdentity
  ): Unit = send(remote, DataDeltas(deltas, filteredDeltas, aclVersion))

  override def sendDeltas(deltas: Seq[BftDelta[Acl]], remote: PublicIdentity): Unit =
    send(remote, AclDeltas(deltas))

  override def requestDeltas(deltas: Dots, remote: PublicIdentity): Unit =
    send(remote, SendMe(deltas, Set.empty))

  override def requestDeltas(deltas: Set[Hash], remote: PublicIdentity): Unit =
    send(remote, SendMe(Dots.empty, deltas))

  override def tellAclVersion(heads: Set[Hash], remote: PublicIdentity): Unit =
    send(remote, MyAclVersionIs(heads))

  override def tellRdtVersion(dots: Dots, remote: PublicIdentity): Unit =
    send(remote, MyRdtVersionIs(dots))

  override def connectedPeers: Set[PublicIdentity] = conn.connectedPeers
}
