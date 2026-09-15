package ex2026accessControl.evaluation

import channels.connection.MessageBuffer
import crypto.PublicIdentity
import replication.sync.ConnectionManager

/** A [[ConnectionManager]] without an actual network layer, which does nothing but sum up the messages handed
  * to it: their total number, as well as their total size in bytes. Used to benchmark the sending side of
  * anti-entropy (i.e. looking up, filtering and encoding what is to be sent) without paying for, or being
  * distorted by, any actual I/O.
  *
  * Not thread safe, as benchmarks drive it from a single thread.
  */
class SummingConnectionManager(override val connectedPeers: Set[PublicIdentity] = Set.empty)
    extends ConnectionManager {

  private var bytes: Long    = 0
  private var messages: Long = 0

  /** Total size in bytes of all messages sent so far. */
  def sentBytes: Long = bytes

  /** Total number of messages sent so far. */
  def sentMessages: Long = messages

  private def sum(msg: MessageBuffer): Unit = {
    bytes += msg.asByteBuffer.remaining()
    messages += 1
  }

  def reset(): Unit = {
    bytes = 0
    messages = 0
  }

  override def send(user: PublicIdentity, msg: MessageBuffer): Unit = sum(msg)

  override def sendMultiple(user: PublicIdentity, msgs: Iterable[MessageBuffer]): Unit = msgs.foreach(sum)

  /** Counts every broadcast message once, independently of the number of connected peers. */
  override def broadcast(msgs: Iterable[MessageBuffer]): Unit = msgs.foreach(sum)

  override def listenAddress: Option[(String, Int)] = None

  override def shutdown(): Unit = ()

  override def acceptIncomingConnections(): Unit = ()

  override def connectTo(host: String, port: Int): Unit = ()

  override def disconnect(userId: PublicIdentity): Unit = ()
}
