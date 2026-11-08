package replication.acl.sync

import channels.{NioTCP, NioTls}
import channels.connection.{Abort, Connection, ConnectionDescriptor, MessageBuffer, Receive}
import channels.crypto.PublicIdentity
import channels.crypto.PrivateIdentity
import de.rmgk.delay.Callback

import java.net.{InetAddress, InetSocketAddress}
import java.nio.channels.ServerSocketChannel
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

class ChannelConnectionManager(
    private val privateIdentity: PrivateIdentity,
    messageReceiver: MessageReceiver[MessageBuffer],
    ifAddress: InetAddress = InetAddress.getLoopbackAddress,
    requestedListenPort: Int = 0,
    val abort: Abort = Abort(),
    disableLogging: Boolean = true
) extends ConnectionManager {
  // one thread runs the selector loop, another delivers received messages (in order), so slow receivers do not
  // block reading from sockets
  private val selectorExecutor: ExecutorService = Executors.newSingleThreadExecutor()
  private val deliveryExecutor: ExecutorService = Executors.newSingleThreadExecutor()
  private def executionContext(executor: ExecutorService): ExecutionContext =
    if disableLogging
    then ExecutionContext.fromExecutor(executor, _ => ())
    else ExecutionContext.fromExecutor(executor)
  private val tls           = NioTls(privateIdentity)
  private val nio           = NioTCP(executionContext(deliveryExecutor))
  private val localPublicId = privateIdentity.getPublic
  @volatile private var listener: Option[(ServerSocketChannel, ConnectionDescriptor.TcpTls)] = None
  selectorExecutor.execute(() => nio.loopSelection(abort))
  // Stores multiple connections
  @volatile private var connections: Map[PublicIdentity, Connection] = Map.empty

  /** Sends a message to the user and returns true, if a connections exists. Otherwise, discards message and returns false.
    *
    * If the ConnectionManager is shut down, this method also returns false.
    *
    * @param remotePeerId The user to send the message to.
    * @param msg The message to send.
    * @return true if a connections exists, otherwise false.
    */
  override inline def send(remotePeerId: PublicIdentity, msg: MessageBuffer): Unit =
    sendMultiple(remotePeerId, Iterable.single(msg))

  override def sendMultiple(remotePeerId: PublicIdentity, messages: Iterable[MessageBuffer]): Unit = {
    if abort.closeRequest then return
    connections.get(remotePeerId) match
        case Some(connection) =>
          messages.foreach { message =>
            connection.send(message).runIn(abort) {
              case Success(_) => if !disableLogging then
                    println(s"Successfully sent msg: ${String(message.convertToArray())} to ${remotePeerId}")
              case Failure(exception) =>
                if !disableLogging then exception.printStackTrace()
                onSocketFailure(remotePeerId, connection)
            }
          }
        case _ =>
  }

  override def broadcast(messages: Iterable[MessageBuffer]): Unit =
    connections.foreach { (user, _) =>
      sendMultiple(user, messages)
    }

  override def listenAddress: Option[(String, Int)] =
    if abort.closeRequest then None
    else listener.map((_, descriptor) => descriptor.host -> descriptor.port)

  override def shutdown(): Unit = synchronized {
    abort.closeRequest = true
    listener.foreach((serverChannel, _) => Try { serverChannel.close() })
    val connectionsToClose = connections
    connections = Map.empty
    connectionsToClose.foreach {
      case (_, connection) => Try { connection.close() }
    }
    nio.selector.wakeup()
    selectorExecutor.shutdown()
    deliveryExecutor.shutdown()
  }

  override def acceptIncomingConnections(): Unit = {
    require(!abort.closeRequest)
    require(listener.isEmpty) // unsafe singleton, should be fine though™
    val serverChannel = nio.defaultServerSocketChannel(InetSocketAddress(ifAddress, requestedListenPort))()
    // binding and registering happen synchronously, so the listener is set when this returns
    nio.listenTls(() => serverChannel, tls).listen { conn =>
      receiveMessageHandler(conn)
    }.runIn(abort) {
      case Success(descriptor: ConnectionDescriptor.TcpTls) =>
        listener = Some(serverChannel -> descriptor)
        if !disableLogging then println(s"Listening on ${descriptor.host}:${descriptor.port} as ${localPublicId}")
      case Success(other) => throw IllegalStateException(s"unexpected listen descriptor: $other")
      case Failure(exception) =>
        // Connection not established yet, don't need to remove/close anything
        Try { serverChannel.close() }
        if !disableLogging then exception.printStackTrace()
    }
  }

  override def connectTo(host: String, port: Int): Unit = {
    if !disableLogging then println(s"Attempting to connect to $host:$port")
    nio.connectTls(nio.defaultSocketChannel(InetSocketAddress(host, port)), tls).connect(
      receiveMessageHandler(_)
    ).runIn(abort) {
      case Success(_)         => ()
      case Failure(exception) => if !disableLogging then exception.printStackTrace()
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

  private def trackConnection(connection: Connection): Unit =
    connection.info.authenticatedPeer.map(id => PublicIdentity(id.delegate)) match {
      case Some(`localPublicId`) =>
        if !disableLogging then println("Refusing attempt to track connection to myself")
        connection.close()
      case Some(remotePeerId) =>
        var duplicateConnection: Option[Connection] = None
        synchronized {
          connections = connections.updatedWith(remotePeerId) {
            case None                => Some(connection)
            case Some(oldConnection) => // Keep session with higher session id
              val oldSessionId = sessionIdentifier(oldConnection, remotePeerId)
              val newSessionId = sessionIdentifier(connection, remotePeerId)
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
                  "Connection established with: " + remotePeerId.id + s" (${sessionIdentifier(connection, remotePeerId)})"
                )
            messageReceiver.connectionEstablished(remotePeerId)
      case None => ??? // Should not happen
    }

  /** Identifies the TCP session underlying `connection`, the same on both sides, used to pick one of two duplicate
    * connections consistently. Assumes the listen port is fixed and the initiator port varies. Not unique, if two
    * replicas share an id.
    */
  private def sessionIdentifier(connection: Connection, remotePeerId: PublicIdentity): String = {
    def port(descriptor: Option[ConnectionDescriptor]): Int = descriptor match
        case Some(ConnectionDescriptor.TcpTls(_, port)) => port
        case _                                          => -1
    val ports   = List(remotePeerId.id -> port(connection.info.remote), localPublicId.id -> port(connection.info.local))
    val minPort = ports.minBy(_._1)._2
    val maxPort = ports.maxBy(_._1)._2
    s"$minPort|$maxPort"
  }

  private def receiveMessageHandler(connection: Connection): Callback[MessageBuffer] = {
    trackConnection(connection)
    val remotePeerId = PublicIdentity(connection.info.authenticatedPeer.get.delegate)

    new Callback[MessageBuffer] {
      def complete(result: Try[MessageBuffer]): Unit = result match {
        case Success(msg)       => messageReceiver.receivedMessage(msg, remotePeerId)
        case Failure(exception) =>
          if !disableLogging
          then println(s"Closing connection with $remotePeerId at ${connection.info}, because of $exception")
          onSocketFailure(remotePeerId, connection)
      }
    }
  }

  private def onSocketFailure(remotePeerId: PublicIdentity, failedConnection: Connection): Unit = {
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
