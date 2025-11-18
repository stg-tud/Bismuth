package ex2024travel.lofi_acl.sync

import channels.{Abort, Connection, MessageBuffer, Receive}
import crypto.channels.P2PTls
import crypto.{CertificatePem, PrivateKeyPem, PublicIdentity}
import de.rmgk.delay.Callback

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
  private val connections: AtomicReference[Map[PublicIdentity, Set[Connection[MessageBuffer]]]] =
    AtomicReference(Map.empty)

  /** Sends a message to the user and returns true, if a connections exists. Otherwise, discards message and returns false.
    *
    * If the ConnectionManager is shut down, this method also returns false.
    *
    * @param user The user to send the message to.
    * @param msg The message to send.
    * @return true if a connections exists, otherwise false.
    */
  override inline def send(user: PublicIdentity, msg: MessageBuffer): Boolean =
    sendMultiple(user, Array(msg))

  override def sendMultiple(user: PublicIdentity, messages: Array[MessageBuffer]): Boolean = {
    if abort.closeRequest then return false
    connections.get.get(user) match
        case Some(connectionSet) if connectionSet.nonEmpty =>
          val connection = connectionSet.head
          messages.foreach { message =>
            connection.send(message).runIn(abort) {
              case Success(_) => if !disableLogging then println(s"Successfully sent msg: ${String(message.asArray)}")
              case Failure(exception) => if !disableLogging then exception.printStackTrace()
            }
          }
          true
        case _ => false
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
      case (_, connections) => connections.foreach(conn => Try { conn.close() })
    }
    executor.shutdownNow(): Unit
  }

  override def acceptIncomingConnections(): Unit = {
    require(!abort.closeRequest)
    require(listener.isEmpty) // unsafe singleton, should be fine though™
    listener = Some(p2pTls.latentListener(0, ec))
    listener.get.prepare(receiveMessageHandler).runIn(abort) {
      case Success(connection) => trackConnection(connection)
      case Failure(exception)  => if !disableLogging then exception.printStackTrace()
    }
  }

  override def connectTo(host: String, port: Int): Unit = {
    p2pTls.latentConnect(host, port, ec).prepare(receiveMessageHandler).runIn(abort) {
      case Success(connection) => trackConnection(connection)
      case Failure(exception)  => if !disableLogging then exception.printStackTrace()
    }
  }

  override def disconnect(userId: PublicIdentity): Unit =
    connections.get().get(userId).foreach(_.foreach(_.close()))

  override def connectedPeers: Set[PublicIdentity] = connections.get().keySet

  private def trackConnection(connection: Connection[MessageBuffer]): Unit = {
    connection.authenticatedPeerReplicaId.map(id => PublicIdentity(id.delegate)) match {
      case Some(`localPublicId`) =>
        if !disableLogging then println("Refusing attempt to track connection to myself")
        connection.close()
      case None               => ??? // Should not happen
      case Some(remotePeerId) =>
        if !disableLogging then println("Connection established with: " + remotePeerId)
        val updated = connections.updateAndGet(old =>
          old.updatedWith(remotePeerId) {
            case None              => Some(Set(connection))
            case Some(connections) => Some(connections + connection)
          }
        )
        if !disableLogging && updated(remotePeerId).size > 1
        then println(s"duplicate connections from $remotePeerId: $connections + $connection")

        messageReceiver.connectionEstablished(remotePeerId)
    }
  }

  private val receiveMessageHandler: Receive[MessageBuffer] = (connection: Connection[MessageBuffer]) => {
    val remotePeerId = PublicIdentity(connection.authenticatedPeerReplicaId.get.delegate)
    {
      case Success(msg)       => messageReceiver.receivedMessage(msg, remotePeerId)
      case Failure(exception) =>
        if !disableLogging then
            println(
              s"Closing connection with ${connection.authenticatedPeerReplicaId.get} at ${connection.info.hostname}:${connection.info.port}, because of $exception"
            )
        connections.updateAndGet { old =>
          old.updatedWith(remotePeerId) {
            case Some(connections) =>
              if connections.size == 1 && connections.head == connection then None
              else Some(connections - connection)
            case None => ??? // Should not happen... right?
          }
        }
        Try {
          messageReceiver.connectionShutdown(remotePeerId)
        }
        Try {
          connection.close()
        }: Unit
    }
  }

}
