package lofi_acl.sync

import channels.MessageBuffer
import crypto.PublicIdentity

import java.util.concurrent.LinkedBlockingQueue

class QueueAppendingMessageReceiver(_id: PublicIdentity = null) extends MessageReceiver[MessageBuffer] {
  private val localId = Option(_id)

  val messageQueue: LinkedBlockingQueue[(MessageBuffer, PublicIdentity)] =
    LinkedBlockingQueue[(MessageBuffer, PublicIdentity)]()

  val connectionQueue: LinkedBlockingQueue[PublicIdentity] = LinkedBlockingQueue()

  override def receivedMessage(msg: MessageBuffer, fromUser: PublicIdentity): Unit =
    messageQueue.put((msg, fromUser))

  override def connectionEstablished(publicIdentity: PublicIdentity): Unit =
    // if DEBUG then println(s"${localId.map(_.id).getOrElse("Replica")} is now connected to ${publicIdentity.id}")
    connectionQueue.put(publicIdentity)

  override def connectionShutdown(publicIdentity: PublicIdentity): Unit =
    if false then println(s"${localId.map(_.id).getOrElse("Replica")} connectionShutdown to $publicIdentity")
}
