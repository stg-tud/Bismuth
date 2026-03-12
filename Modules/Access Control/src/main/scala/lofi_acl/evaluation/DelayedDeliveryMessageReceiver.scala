package lofi_acl.evaluation

import crypto.PublicIdentity
import lofi_acl.sync.MessageReceiver

import java.util.concurrent.Executors

object DelayedDeliveryMessageReceiver {
  def delay[MSG](delay: Int, messageReceiver: MessageReceiver[MSG]): MessageReceiver[MSG] = new MessageReceiver[MSG]:
      private val threadPool = Executors.newCachedThreadPool()
      threadPool.execute { () => }

      override def receivedMessage(msg: MSG, fromUser: PublicIdentity): Unit = threadPool.execute { () =>
        Thread.sleep(delay)
        messageReceiver.receivedMessage(msg, fromUser)
      }: Unit
      override def connectionShutdown(publicIdentity: PublicIdentity): Unit =
        messageReceiver.connectionShutdown(publicIdentity)
      override def connectionEstablished(publicIdentity: PublicIdentity): Unit =
        messageReceiver.connectionEstablished(publicIdentity)

}
