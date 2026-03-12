package lofi_acl.evaluation

import crypto.PublicIdentity
import lofi_acl.sync.MessageReceiver

object DelayedDeliveryMessageReceiver {
  def delay[MSG](delay: Int, messageReceiver: MessageReceiver[MSG]): MessageReceiver[MSG] = new MessageReceiver[MSG]:
      override def receivedMessage(msg: MSG, fromUser: PublicIdentity): Unit = Thread.ofVirtual().start { () =>
        Thread.sleep(delay)
        messageReceiver.receivedMessage(msg, fromUser)
      }: Unit
      override def connectionShutdown(publicIdentity: PublicIdentity): Unit =
        messageReceiver.connectionShutdown(publicIdentity)
      override def connectionEstablished(publicIdentity: PublicIdentity): Unit =
        messageReceiver.connectionEstablished(publicIdentity)

}
