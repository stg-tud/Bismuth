package replication.acl.sync.anti_entropy

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import crypto.{Hash, PublicIdentity}
import AclEnforcingSync.SyncMsg
import AclEnforcingSync.SyncMsg.*
import channels.connection.ByteBufferMessageBuffer
import rdts.time.Dots
import replication.acl.{Acl, BftDelta}
import replication.acl.sync.ConnectionManager

class ConnectionManagerCommunicator[State](private val conn: ConnectionManager)(using JsonValueCodec[SyncMsg[State]])
    extends AntiEntropyCommunicator[State] {

  protected def send(remote: PublicIdentity, syncMsg: SyncMsg[State]): Unit = {
    val buffer = ByteBufferMessageBuffer(writeToArray(syncMsg))
    conn.send(remote, buffer)
  }

  override def sendDeltas(
      deltas: Seq[SignedDelta[State]],
      filteredDeltas: Dots,
      aclVersion: Set[Hash],
      remote: PublicIdentity
  ): Unit = {
    val msg = DataDeltas(deltas, filteredDeltas, aclVersion)
    // Debug.sent(msg, remote)
    send(remote, msg)
  }

  override def sendDeltas(deltas: Seq[BftDelta[Acl]], remote: PublicIdentity): Unit = {
    val msg: SyncMsg[State] = AclDeltas(deltas)
    // Debug.sent(msg, remote)
    send(remote, msg)
  }

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
