package lofi_acl.sync

import channels.{Abort, Connection, MessageBuffer, Receive}
import crypto.channels.P2PTls
import crypto.{CertificatePem, PrivateKeyPem, PublicIdentity}
import de.rmgk.delay.Callback
import lofi_acl.travelplanner.Debug

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

class ChannelConnectionManager(
    tlsKeyPem: PrivateKeyPem,
    tlsCertPem: CertificatePem,
    localPublicId: PublicIdentity,
    messageReceiver: MessageReceiver[MessageBuffer],
    val abort: Abort = Abort(),
    disableLogging: Boolean = true
) extends ConnectionManager {
  private val executor: ExecutorService = Executors.newCachedThreadPool()
  private given ec: ExecutionContext    = {
    if disableLogging
    then ExecutionContext.fromExecutor(executor, _ => ())
    else ExecutionContext.fromExecutor(executor)
  }
  private val p2pTls                                            = P2PTls(tlsKeyPem, tlsCertPem)
  @volatile private var listener: Option[p2pTls.P2PTlsListener] = None
  // Stores multiple connections
  private val connections: AtomicReference[Map[PublicIdentity, Connection[MessageBuffer]]] =
    AtomicReference(Map.empty)

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
    connections.get.get(remotePeerId) match
        case Some(connection) =>
          messages.foreach { message =>
            connection.send(message).runIn(abort) {
              case Success(_) => if !disableLogging then println(s"Successfully sent msg: ${String(message.asArray)}")
              case Failure(exception) =>
                if !disableLogging then exception.printStackTrace()
                onSocketFailure(remotePeerId, connection)
            }
          }
        case _ =>
  }

  override def broadcast(messages: Array[MessageBuffer]): Unit = {
    connections.get.foreach { (user, connection) =>
      sendMultiple(user, messages)
    }
  }

  override def listenPort: Option[Int] = {
    if abort.closeRequest then None
    else listener.map(_.listenPort)
  }

  override def shutdown(): Unit = {
    abort.closeRequest = true
    val old = connections.getAndUpdate(old => Map.empty)
    old.foreach {
      case (_, connection) => Try { connection.close() }
    }
    executor.shutdownNow(): Unit
  }

  override def acceptIncomingConnections(): Unit = {
    require(!abort.closeRequest)
    require(listener.isEmpty) // unsafe singleton, should be fine though™
    listener = Some(p2pTls.latentListener(0, ec))
    Debug.log("Listening on " + listener.get.listenPort)
    listener.get.prepare(receiveMessageHandler).runIn(abort) {
      case Success(connection) => trackConnection(connection)
      case Failure(exception)  =>
        // Connection not established yet, don't need to remove/close anything
        if !disableLogging then exception.printStackTrace()
    }
  }

  override def connectTo(host: String, port: Int): Unit = {
    Debug.log(s"Attempting to connect to $host:$port")
    p2pTls.latentConnect(host, port, ec).prepare(receiveMessageHandler).runIn(abort) {
      case Success(connection) => trackConnection(connection)
      case Failure(exception)  => if !disableLogging then exception.printStackTrace()
    }
  }

  override def disconnect(userId: PublicIdentity): Unit =
    connections.get().get(userId).foreach(_.close())

  override def connectedPeers: Set[PublicIdentity] = connections.get().keySet

  override def peerAddresses: Map[PublicIdentity, (String, Int)] =
    connections.get().map { (remote, connection) =>
      remote -> (connection.info.details("host"), connection.info.details("port").toInt)
    }

  private def trackConnection(connection: Connection[MessageBuffer]): Unit = {
    connection.authenticatedPeerReplicaId.map(id => PublicIdentity(id.delegate)) match {
      case Some(`localPublicId`) =>
        if !disableLogging then println("Refusing attempt to track connection to myself")
        connection.close()
      case Some(remotePeerId) =>
        if !disableLogging
        then
            println(
              "Connection established with: " + remotePeerId.id + s" (${connection.info.details("hacky_identifier")})"
            )
        var duplicateConnection: Option[Connection[MessageBuffer]] = None
        val updated                                                = connections.updateAndGet(old =>
          old.updatedWith(remotePeerId) {
            case None                => Some(connection)
            case Some(oldConnection) => // Keep session with higher session id
              val oldSessionId = oldConnection.info.details("hacky_identifier")
              val newSessionId = connection.info.details("hacky_identifier")
              assert(oldSessionId != newSessionId)
              if oldSessionId.toInt < newSessionId.toInt
              then
                  duplicateConnection = Some(oldConnection)
                  Some(connection)
              else
                  duplicateConnection = Some(connection)
                  Some(oldConnection)
          }
        )
        duplicateConnection.foreach { connectionToBeClosed =>
          Try { connectionToBeClosed.close() }
          println(s"duplicate connections from $remotePeerId: $connections + $connection")
        }
        if duplicateConnection.isEmpty then messageReceiver.connectionEstablished(remotePeerId)
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
    connections.updateAndGet { old =>
      old.updatedWith(remotePeerId) {
        case Some(connection) =>
          if connection == failedConnection then None
          else
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
