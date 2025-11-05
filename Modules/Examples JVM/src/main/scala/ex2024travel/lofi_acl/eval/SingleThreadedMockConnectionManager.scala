package ex2024travel.lofi_acl.eval

import channels.MessageBuffer
import crypto.PublicIdentity
import ex2024travel.lofi_acl.sync.ConnectionManager

class MockConnectionRegistry {
  var connectionManagers: Map[(String, Int), SingleThreadedMockConnectionManager] = Map.empty

  def getPeer(hostname: String, port: Int): Option[SingleThreadedMockConnectionManager] =
    connectionManagers.get((hostname, port))

  def addMyself(peer: SingleThreadedMockConnectionManager): (String, Int) = {
    val address = ("mock", nextFreePort)
    nextFreePort += 1
    connectionManagers = connectionManagers.updated(address, peer)
    address
  }

  private var nextFreePort = 0
}

class SingleThreadedMockConnectionManager(
    val localUserId: PublicIdentity,
    val receiveHandler: (PublicIdentity, MessageBuffer) => Unit
)(using registry: MockConnectionRegistry) extends ConnectionManager {
  var peers: Map[PublicIdentity, SingleThreadedMockConnectionManager] = Map.empty

  /** Sends a message to the user and returns true, if a connections exists. Otherwise, discards message and returns false.
    *
    * If the ConnectionManager is shut down, this method also returns false.
    *
    * @param user The user to send the message to.
    * @param msg  The message to send.
    * @return true if a connections exists, otherwise false.
    */
  override def send(user: PublicIdentity, msg: MessageBuffer): Boolean = {
    val peer = peers.get(user)
    peer.foreach(peer => peer.receiveHandler(localUserId, msg))
    peer.isEmpty
  }

  override def sendMultiple(user: PublicIdentity, messages: MessageBuffer*): Boolean =
    messages.forall(msg => send(user, msg))

  override def broadcast(messages: MessageBuffer*): Unit =
    peers.foreach { (id, peer) =>
      messages.foreach { msg =>
        peer.receiveHandler(localUserId, msg)
      }
    }

  var listenPort: Option[Int] = None

  override def shutdown(): Unit = {
    peers.foreach { (_, peer) =>
      peer.peers = peer.peers.removed(localUserId)
    }
    peers = Map.empty
  }

  override def acceptIncomingConnections(): Unit =
    listenPort = Some(registry.addMyself(this)._2)

  override def connectTo(host: String, port: Int): Unit = {
    registry.getPeer(host, port) match {
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
