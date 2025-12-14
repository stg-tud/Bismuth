package lofi_acl.sync.bft.eval

import channels.MessageBuffer
import crypto.PublicIdentity
import lofi_acl.sync.{ConnectionManager, MessageReceiver}

class MockConnectionRegistry {
  private var connectionManagers: Map[(String, Int), MockConnectionManager] = Map.empty
  private var addresses: Map[PublicIdentity, (String, Int)]                 = Map.empty

  def peer(hostname: String, port: Int): Option[MockConnectionManager] =
    connectionManagers.get((hostname, port))

  def address(peer: PublicIdentity): (String, Int) = addresses(peer)

  def addMyself(peer: MockConnectionManager): (String, Int) = {
    val address = ("mock", nextFreePort)
    nextFreePort += 1
    connectionManagers = connectionManagers.updated(address, peer)
    addresses = addresses + (peer.localUserId -> address)
    address
  }

  private var nextFreePort = 0
}

class MockConnectionManager(
    val localUserId: PublicIdentity,
    val messageReceiver: MessageReceiver[MessageBuffer],
)(using registry: MockConnectionRegistry) extends ConnectionManager {
  @volatile var peers: Map[PublicIdentity, MockConnectionManager] = Map.empty

  /** Sends a message to the user and returns true, if a connections exists. Otherwise, discards message and returns false.
    *
    * If the ConnectionManager is shut down, this method also returns false.
    *
    * @param user The user to send the message to.
    * @param msg  The message to send.
    * @return true if a connections exists, otherwise false.
    */
  override def send(user: PublicIdentity, msg: MessageBuffer): Boolean = {
    peers.get(user) match {
      case Some(peer) =>
        peer.messageReceiver.receivedMessage(msg, localUserId)
        true
      case None => false
    }
  }

  override def sendMultiple(user: PublicIdentity, messages: Array[MessageBuffer]): Boolean =
    peers.get(user) match {
      case Some(peer) =>
        messages.foreach { msg => peer.messageReceiver.receivedMessage(msg, localUserId) }
        true
      case None => false
    }

  override def broadcast(messages: Array[MessageBuffer]): Unit =
    peers.foreach { (id, peer) =>
      messages.foreach { msg =>
        peer.messageReceiver.receivedMessage(msg, localUserId)
      }
    }

  var listenPort: Option[Int] = None

  override def shutdown(): Unit = {
    peers.foreach { (_, peer) =>
      peer.peers = peer.peers.removed(localUserId)
      peer.messageReceiver.connectionShutdown(localUserId)
    }
    peers = Map.empty
  }

  override def acceptIncomingConnections(): Unit =
    listenPort = Some(registry.addMyself(this)._2)

  override def connectTo(host: String, port: Int): Unit = {
    registry.peer(host, port) match {
      case Some(peer) =>
        if !peers.contains(peer.localUserId) then {
          require(!peer.peers.contains(localUserId))
          peers = peers.updated(peer.localUserId, peer)
          peer.peers = peer.peers.updated(localUserId, this)
        }
      case None => ()
    }
  }

  override def disconnect(remoteUserId: PublicIdentity): Unit =
    peers.get(remoteUserId).foreach { peer =>
      peers = peers.removed(remoteUserId)
      peer.peers = peer.peers.removed(localUserId)
    }

  override def connectedPeers: Set[PublicIdentity] = peers.keySet
}
