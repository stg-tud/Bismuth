package ex2024travel.lofi_acl.sync

import channels.MessageBuffer
import crypto.PublicIdentity

trait ConnectionManager {

  /** Sends a message to the user and returns true, if a connections exists. Otherwise, discards message and returns false.
    *
    * If the ConnectionManager is shut down, this method also returns false.
    *
    * @param user The user to send the message to.
    * @param msg  The message to send.
    * @return true if a connections exists, otherwise false.
    */
  def send(user: PublicIdentity, msg: MessageBuffer): Boolean

  def sendMultiple(user: PublicIdentity, messages: MessageBuffer*): Boolean

  def broadcast(messages: MessageBuffer*): Unit

  def listenPort: Option[Int]

  def shutdown(): Unit

  def acceptIncomingConnections(): Unit

  def connectTo(host: String, port: Int): Unit

  def disconnect(userId: PublicIdentity): Unit

  def connectedPeers: Set[PublicIdentity]
}
