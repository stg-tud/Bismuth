package channels
package connection

import channels.connection.NioTCP.*
import de.rmgk.delay.Async

import java.net.{InetSocketAddress, SocketAddress, StandardProtocolFamily, StandardSocketOptions, UnixDomainSocketAddress}
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

object NioTCP {

  case class AcceptAttachment(
      incoming: Receive,
  )

  class EndOfChannelException(msg: String) extends Exception(msg)
}

/** Selector-based TCP transport.
  *
  * In addition to the original length-prefixed NioTCP framing, this implementation can accept websocket clients on the
  * same listening socket. Each accepted connection is sniffed once, then handled either as plain NioTCP or as
  * websocket traffic. Different connections may use different protocols concurrently.
  *
  * Layers: [[WebsocketMultiplexing]] is what is said on one socket, [[NioConnection]] runs it on one socket, this
  * multiplexes all sockets on one selector.
  *
  * [[loopSelection]] and [[runSelection]] should not be called from multiple threads at the same time.
  * Connections may be sent on from any thread (including the selector thread, e.g., from receive callbacks): sending
  * does not wait for the socket, what it does not take right away is written by the selector thread later
  * (see [[NioConnection]]).
  */
class NioTCP(accepCallbackExecutor: ExecutionContext = MessagingIO.executeImmediately) {
  inline val compression: false = false

  val selector: Selector = Selector.open()

  def loopSelection(abort: Abort): Unit =
    while !abort.closeRequest do
        selector.select(1000)
        runSelection()

  def runSelection(): Unit = {

    selector.selectedKeys().forEach { key =>
      try
        key.attachment() match
            case conn: NioConnection =>
              if key.isValid && key.isWritable then conn.continueWriting()
              if key.isValid && key.isReadable then conn.receiveAvailable()
            case accept: AcceptAttachment =>
              if key.isValid && key.isAcceptable then
                  val clientChannel = key.channel().asInstanceOf[ServerSocketChannel].accept()
                  if clientChannel != null then {
                    configureChannel(clientChannel)
                    NioConnection.accepted(clientChannel, accept.incoming, accepCallbackExecutor).register(selector)
                  }
      catch {
        case ex: Exception =>
          failKey(key, ex)
      }
    }

    selector.selectedKeys().clear()
  }

  /** Connects using the channel of `bindsocket`, which must be non-blocking. */
  def connect(
      bindsocket: () => SocketChannel,
  ): Connector =
    new Connector {
      override def connect(incoming: Receive): Async[Any, Connection] =
        Async.fromCallback {
          try
              val conn = handleConnectTo(bindsocket(), incoming)
              Async.handler.succeed(conn)
          catch case NonFatal(exception) => Async.handler.fail(exception)
        }
    }

  /** Connects as the websocket client of a server at `host` (for the `Host` header), for `path`, using the channel of
    * `bindsocket`, which must be non-blocking.
    */
  def connectWebSocket(bindsocket: () => SocketChannel, host: String, path: String = "/"): Connector =
    new Connector {
      override def connect(incoming: Receive): Async[Any, Connection] =
        MultiplexedConnection.connectWebsocketClient(incoming, host, path)(
          create = established => NioConnection.accepted(bindsocket(), established, accepCallbackExecutor),
          start = conn => conn.register(selector)
        )
    }

  def handleConnectTo(
      clientChannel: SocketChannel,
      incoming: Receive,
  ): NioConnection = {

    val conn = NioConnection.dialed(clientChannel, incoming, accepCallbackExecutor)
    conn.register(selector)
    conn
  }

  /** Unix domain sockets, or TCP over IPv6 and IPv4 (the default protocol family, not only IPv4). */
  def defaultSocketChannel(socketAddress: SocketAddress): () => SocketChannel = () => {
    val channel = socketAddress match
        case _: UnixDomainSocketAddress => SocketChannel.open(StandardProtocolFamily.UNIX)
        case _                          => SocketChannel.open()
    channel.connect(socketAddress)
    configureChannel(channel)
    channel
  }

  private def configureChannel(channel: SocketChannel) = {
    channel.configureBlocking(false)
    try
        channel.setOption(StandardSocketOptions.TCP_NODELAY, true)
    catch
        case _: UnsupportedOperationException => // fine
  }

  /** Unix domain sockets, or TCP over IPv6 and IPv4 (the default protocol family: a wildcard address accepts both). */
  def defaultServerSocketChannel(socketAddress: SocketAddress): () => ServerSocketChannel = () => {
    val socket = socketAddress match
        case _: UnixDomainSocketAddress => ServerSocketChannel.open(StandardProtocolFamily.UNIX)
        case _                          => ServerSocketChannel.open()
    socket.configureBlocking(false)
    socket.bind(socketAddress)
    socket
  }

  def listen(): Listener =
    listen(
      defaultServerSocketChannel(new InetSocketAddress(0))
    )

  def listen(
      bindsocket: () => ServerSocketChannel,
  ): Listener =
    new Listener {
      override def listen(incoming: Receive): Async[Abort, ConnectionDescriptor] =
        Async.fromCallback { abort ?=>
          try {
            val serverChannel: ServerSocketChannel = bindsocket()

            val descriptor = serverChannel.getLocalAddress match {
              case isa: InetSocketAddress => Some(ConnectionDescriptor.TcpWebSocket(isa.getHostString, isa.getPort))
              case uda: UnixDomainSocketAddress => Some(ConnectionDescriptor.Unix(uda.getPath.toString))
              case _                            => None
            }

            descriptor match
                case Some(descriptor) =>
                  serverChannel.register(selector, SelectionKey.OP_ACCEPT, AcceptAttachment(incoming))
                  selector.wakeup()
                  Async.handler.succeed(descriptor)
                case None =>
                  Async.handler.fail(
                    IllegalStateException(s"unsupported socket address ${serverChannel.getLocalAddress}")
                  )

            ()
          } catch
              case NonFatal(ex) => Async.handler.fail(ex)

        }
    }

  private def failKey(key: SelectionKey, ex: Exception): Unit = {
    val channel = key.channel()
    try channel.close()
    catch case NonFatal(_) => ()
    key.cancel()
    key.attachment() match {
      case conn: NioConnection => conn.closeWithFailure(ex)
      case _                   => ()
    }
  }
}
