package ex2024travel.lofi_acl.sync

import crypto.PublicIdentity

trait MessageReceiver[MSG]:
   def receivedMessage(msg: MSG, fromUser: PublicIdentity): Unit
   def connectionEstablished(publicIdentity: PublicIdentity): Unit = {}
   def connectionShutdown(publicIdentity: PublicIdentity): Unit    = {}

object MessageReceiver:
   def wrap[A, B](messageReceiver: MessageReceiver[B], transform: A => B): MessageReceiver[A] = new MessageReceiver[A]:
      override def receivedMessage(msg: A, fromUser: PublicIdentity): Unit =
        messageReceiver.receivedMessage(transform(msg), fromUser)
      override def connectionEstablished(publicIdentity: PublicIdentity): Unit =
        messageReceiver.connectionEstablished(publicIdentity)
      override def connectionShutdown(publicIdentity: PublicIdentity): Unit =
        messageReceiver.connectionShutdown(publicIdentity)
