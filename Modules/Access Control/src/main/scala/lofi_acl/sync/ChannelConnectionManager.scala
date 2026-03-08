package lofi_acl.sync

import channels.{Abort, Connection, MessageBuffer, Receive}
import crypto.PublicIdentity
import crypto.channels.{P2PTls, PrivateIdentity}
import de.rmgk.delay.Callback
import lofi_acl.Debug

import java.net.InetAddress
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

class ChannelConnectionManager(
    private val privateIdentity: PrivateIdentity,
    messageReceiver: MessageReceiver[MessageBuffer],
    ifAddress: InetAddress = InetAddress.getLoopbackAddress,
    val abort: Abort = Abort(),
    disableLogging: Boolean = true
) extends ConnectionManager {
  private val executor: ExecutorService = Executors.newCachedThreadPool()
  private given ec: ExecutionContext    = {
    if disableLogging
    then ExecutionContext.fromExecutor(executor, _ => ())
    else ExecutionContext.fromExecutor(executor)
  }
  private val p2pTls                                            = P2PTls(privateIdentity)
  private val localPublicId                                     = privateIdentity.getPublic
  @volatile private var listener: Option[p2pTls.P2PTlsListener] = None
  // Stores multiple connections
  @volatile private var connections: Map[PublicIdentity, Connection[MessageBuffer]] = Map.empty

  /** Sends a message to the user and returns true, if a connections exists. Otherwise, discards message and returns false.
    *
    * If the ConnectionManager is shut down, this method also returns false.
    *
    * @param remotePeerId The user to send the message to.
    * @param msg The message to send.
    * @return true if a connections exists, otherwise false.
    */
  override inline def send(remotePeerId: PublicIdentity, msg: MessageBuffer): Unit =
    sendMultiple(remotePeerId, Array(msg))

  override def sendMultiple(remotePeerId: PublicIdentity, messages: Array[MessageBuffer]): Unit = {
    if abort.closeRequest then return
    connections.get(remotePeerId) match
        case Some(connection) =>
          messages.foreach { message =>
            connection.send(message).runIn(abort) {
              case Success(_) => if !disableLogging then
                    println(s"Successfully sent msg: ${String(message.asArray)} to ${Debug.shorten(remotePeerId)}")
              case Failure(exception) =>
                if !disableLogging then exception.printStackTrace()
                onSocketFailure(remotePeerId, connection)
            }
          }
        case _ =>
  }

  override def broadcast(messages: Array[MessageBuffer]): Unit = {
    connections.foreach { (user, connection) =>
      sendMultiple(user, messages)
    }
  }

  override def listenAddress: Option[(String, Int)] = {
    if abort.closeRequest then None
    else listener.map(listener => listener.ifAddress.getHostAddress -> listener.listenPort)
  }

  override def shutdown(): Unit = synchronized {
    abort.closeRequest = true
    listener.get.close()
    val connectionsToClose = connections
    connections = Map.empty
    connectionsToClose.foreach {
      case (_, connection) => Try { connection.close() }
    }
    executor.shutdownNow(): Unit
  }

  override def acceptIncomingConnections(): Unit = {
    require(!abort.closeRequest)
    require(listener.isEmpty) // unsafe singleton, should be fine though™
    listener = Some(p2pTls.latentListener(ifAddress, 0, ec))
    if !disableLogging then
        println(
          s"Listening on ${listener.get.ifAddress.getHostAddress}:${listener.get.listenPort} as ${Debug.shorten(localPublicId)}"
        )
    listener.get.prepare(receiveMessageHandler).runIn(abort) {
      case Success(connection) => trackConnection(connection)
      case Failure(exception)  =>
        // Connection not established yet, don't need to remove/close anything
        if !disableLogging then exception.printStackTrace()
    }
  }

  override def connectTo(host: String, port: Int): Unit = {
    if !disableLogging then println(s"Attempting to connect to $host:$port")
    p2pTls.latentConnect(host, port, ec).prepare(receiveMessageHandler).runIn(abort) {
      case Success(connection) => trackConnection(connection)
      case Failure(exception)  => if !disableLogging then exception.printStackTrace()
    }
  }

  override def disconnect(remotePeerId: PublicIdentity): Unit = synchronized {
    val toClose = connections.get(remotePeerId)
    connections = connections.removed(remotePeerId)
    toClose match {
      case Some(conn) =>
        Try { conn.close() }
        messageReceiver.connectionShutdown(remotePeerId)
      case None =>
    }
  }

  override def connectedPeers: Set[PublicIdentity] = connections.keySet

  private def trackConnection(connection: Connection[MessageBuffer]): Unit = {
    connection.authenticatedPeerReplicaId.map(id => PublicIdentity(id.delegate)) match {
      case Some(`localPublicId`) =>
        if !disableLogging then println("Refusing attempt to track connection to myself")
        connection.close()
      case Some(remotePeerId) =>
        var duplicateConnection: Option[Connection[MessageBuffer]] = None
        synchronized {
          connections = connections.updatedWith(remotePeerId) {
            case None                => Some(connection)
            case Some(oldConnection) => // Keep session with higher session id
              val oldSessionId = oldConnection.info.details("hackyIdentifier")
              val newSessionId = connection.info.details("hackyIdentifier")
              if oldSessionId < newSessionId
              then
                  duplicateConnection = Some(oldConnection)
                  Some(connection)
              else
                  duplicateConnection = Some(connection)
                  Some(oldConnection)
          }
        }
        duplicateConnection.foreach { connectionToBeClosed =>
          Try { connectionToBeClosed.close() }
          if !disableLogging then println(s"duplicate connections from $remotePeerId: $connections + $connection")
        }
        if duplicateConnection.isEmpty then
            if !disableLogging then
                println(
                  "Connection established with: " + remotePeerId.id + s" (${connection.info.details("hackyIdentifier")})"
                )
            messageReceiver.connectionEstablished(remotePeerId)
      case None => ??? // Should not happen
    }
  }

  private val receiveMessageHandler: Receive[MessageBuffer] = (connection: Connection[MessageBuffer]) => {
    val remotePeerId = PublicIdentity(connection.authenticatedPeerReplicaId.get.delegate)
    {
      case Success(msg)       => messageReceiver.receivedMessage(msg, remotePeerId)
      case Failure(exception) =>
        if !disableLogging
        then println(s"Closing connection with $remotePeerId at ${connection.info}, because of $exception")
        onSocketFailure(remotePeerId, connection)
    }
  }

  private def onSocketFailure(remotePeerId: PublicIdentity, failedConnection: Connection[MessageBuffer]): Unit = {
    var socketWasAlreadyRemoved = false
    synchronized {
      connections = connections.updatedWith(remotePeerId) {
        case Some(connection) =>
          if connection == failedConnection then None
          else
              // Keep existing connection in map, as it is different to the failed one
              socketWasAlreadyRemoved = true
              Some(connection)
        case None =>
          socketWasAlreadyRemoved = true
          None // Already removed
      }
    }
    if !socketWasAlreadyRemoved then
        Try { failedConnection.close() }
        Try { messageReceiver.connectionShutdown(remotePeerId) }: Unit
  }

}
