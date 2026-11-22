package channels
package connection

import channels.connection.NioTCP.*
import channels.connection.{Abort, ByteBufferMessageBuffer, Connection, ConnectionDescriptor, ConnectionInfo, Connector, Listener, MessageBuffer, NoMoreDataException, ReceiveBuffer, Receive}
import de.rmgk.delay.{Async, Callback}

import java.net.{InetSocketAddress, SocketAddress, StandardProtocolFamily, StandardSocketOptions, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object NioTCP {

  case class AcceptAttachment(
      incoming: Receive,
  )

  /** Per-socket protocol mode.
    *
    * New inbound sockets start in `Init` so we can sniff the first bytes.
    * After that, each connection independently continues either as normal length-prefixed NioTCP traffic,
    * or as a websocket handshake followed by websocket frames.
    */
  enum ProtocolState {
    case Init
    case Plain
    case WebSocketHandshake

    /** `fragments` of a message whose last frame did not arrive yet. */
    case WebSocket(fragments: Vector[Array[Byte]])

    /** No more data is processed. */
    case Closed
  }

  /** How messages are framed on the byte stream. */
  enum Framing {
    case Plain, WebSocket
  }

  /** Per-connection state stored on the selector key, used by plain NioTCP and websocket traffic alike.
    *
    *   - `buffer` stores received bytes not yet parsed, across selector turns
    *   - `messageCallback` is populated once the connection is established. For inbound accepted sockets, this
    *     is only once the protocol is known
    *   - `layer` is the byte stream all protocols read from and write to
    *
    * Only accessed by the selector thread.
    */
  final class ReceiveAttachment(
      val incoming: Receive,
      val layer: NioSocketLayer,
      var messageCallback: Callback[MessageBuffer] | Null = null,
  ) {
    val buffer: ReceiveBuffer   = ReceiveBuffer()
    var protocol: ProtocolState = ProtocolState.Init
  }

  class EndOfChannelException(msg: String) extends Exception(msg)

  extension (broadcast: MessagingIO[?])
      def serveNioLoop(): Unit = {
        val niotcp = new NioTCP()
        broadcast.addServerConnection(niotcp.listen(niotcp.defaultServerSocketChannel(new InetSocketAddress(0))))
        niotcp.loopSelection(broadcast.globalAbort)
      }

}

/** Selector-based TCP transport.
  *
  * In addition to the original length-prefixed NioTCP framing, this implementation can accept websocket clients on the
  * same listening socket. Each accepted connection is sniffed once, then handled either as plain NioTCP or as
  * websocket traffic. Different connections may use different protocols concurrently.
  *
  * [[loopSelection]] and [[runSelection]] should not be called from multiple threads at the same time.
  * Connections may be sent on from any thread (including the selector thread, e.g., from receive callbacks): sending
  * does not wait for the socket, what it does not take right away is written by the selector thread later
  * (see [[NioSocketLayer]]).
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
      try {
        if key.isValid && key.isWritable then key.attachment().asInstanceOf[ReceiveAttachment].layer.flush()
        if key.isValid && key.isReadable then
            handleReadable(key, key.attachment().asInstanceOf[ReceiveAttachment])
        else if key.isValid && key.isAcceptable then
            val serverChannel = key.channel().asInstanceOf[ServerSocketChannel]
            val attachment    = key.attachment().asInstanceOf[AcceptAttachment]
            val clientChannel = serverChannel.accept()
            if clientChannel != null then {
              configureChannel(clientChannel)
              register(ReceiveAttachment(attachment.incoming, NioSocketLayer(clientChannel)))
            }
      } catch {
        case ex: Exception =>
          failKey(key, ex)
      }
    }

    selector.selectedKeys().clear()
  }

  /** Registers the socket of `attachment` with the selector (for reading, and writing what its layer has queued). */
  private def register(attachment: ReceiveAttachment): Unit = {
    val key = attachment.layer.channel.register(selector, SelectionKey.OP_READ, attachment)
    attachment.layer.registered(key)
    selector.wakeup(): Unit
  }

  /** Writes `buffers` on `layer`, completes once the socket took all of them: senders waiting for that before sending
    * more get backpressure.
    */
  private def writeAsync(layer: NioSocketLayer, buffers: Array[ByteBuffer]): Async[Any, Unit] =
    Async.fromCallback {
      val handler = Async.handler[Unit]
      try layer.write(buffers, handler.complete)
      catch case NonFatal(ex) => handler.fail(ex)
    }

  /** A connection on `layer`, sending messages framed by `framing`. */
  class NioConnection(layer: NioSocketLayer, framing: Framing) extends Connection {

    override val info: ConnectionInfo = layer.connectionInfo("type" -> (framing match
        case Framing.Plain     => "niotcp"
        case Framing.WebSocket => "websocket"))

    override def send(message: MessageBuffer): Async[Any, Unit] = {
      val payload = message.asByteBuffer
      val header  = framing match
          case Framing.Plain     => ByteBuffer.allocate(4).putInt(0, payload.remaining())
          case Framing.WebSocket => ByteBuffer.wrap(WebsocketProtocol.frameHeader(0x2, payload.remaining()))
      writeAsync(layer, Array(header, payload))
    }

    override def close(): Unit = {
      if framing == Framing.WebSocket then
          try layer.write(Array(ByteBuffer.wrap(WebsocketProtocol.encodeCloseFrame())))
          catch case NonFatal(_) => ()
      layer.close()
    }
  }

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

  def handleConnectTo(
      clientChannel: SocketChannel,
      incoming: Receive,
  ): NioConnection = {

    configureChannel(clientChannel)

    val layer    = NioSocketLayer(clientChannel)
    val conn     = NioConnection(layer, Framing.Plain)
    val callback = incoming.connectionEstablished(conn)
    register(ReceiveAttachment(incoming, layer, callback))

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

  /** Reads what the socket has, and handles all complete messages. */
  private def handleReadable(key: SelectionKey, attachment: ReceiveAttachment): Unit = {
    var read = 0
    // read > 0: the socket may have more than fit into the buffer, keep reading until it returns 0
    while
        read = attachment.buffer.fill(attachment.layer.read)
        while attachment.protocol != ProtocolState.Closed && handleBuffered(key, attachment) do ()
        read > 0 && attachment.protocol != ProtocolState.Closed
    do ()
    if read == -1 then throw NoMoreDataException("remaining channel is empty")
  }

  /** Handles the next complete unit (message, frame, handshake) in the buffer.
    * Returns false if the buffer does not contain one yet.
    */
  private def handleBuffered(key: SelectionKey, attachment: ReceiveAttachment): Boolean = {
    val buffer = attachment.buffer
    attachment.protocol match {
      case ProtocolState.Init =>
        if buffer.available < 4 then false
        else {
          attachment.protocol =
            if buffer.peekInt() == WebsocketProtocol.handshakePrefix
            then ProtocolState.WebSocketHandshake
            else
                establish(attachment, Framing.Plain)
                ProtocolState.Plain
          true
        }

      case ProtocolState.Plain =>
        if buffer.available < 4 then false
        else {
          val len = buffer.peekInt()
          require(len < MessageBuffer.maxPayloadSize, "message too large")
          if buffer.available < 4 + len then
              buffer.reserve(4 + len)
              false
          else
              buffer.consume(4)
              deliver(attachment, buffer.take(len))
              true
        }

      case ProtocolState.WebSocketHandshake =>
        val view = buffer.view
        WebsocketProtocol.tryParseHandshake(view) match {
          case None          => false
          case Some(request) =>
            buffer.consume(view.position())
            attachment.layer.write(Array(ByteBuffer.wrap(WebsocketProtocol.handshakeResponse(request))))
            establish(attachment, Framing.WebSocket)
            attachment.protocol = ProtocolState.WebSocket(Vector.empty)
            true
        }

      case ProtocolState.WebSocket(fragments) =>
        val view = buffer.view
        WebsocketProtocol.tryParseHeader(view) match {
          case None         => false
          case Some(header) =>
            require(header.len < MessageBuffer.maxPayloadSize, "message too large")
            val headerLength = view.position()
            if buffer.available < headerLength + header.len then
                buffer.reserve(headerLength + header.len)
                false
            else
                buffer.consume(headerLength)
                val frame = WebsocketProtocol.tryParsePayload(header, buffer.take(header.len))
                attachment.protocol = handleWebsocketFrame(header.fin, frame, attachment, key, fragments)
                true
        }

      case ProtocolState.Closed => false
    }
  }

  /** Publishes the connection of an accepted socket, once its protocol is known. */
  private def establish(attachment: ReceiveAttachment, framing: Framing): Unit =
    if attachment.messageCallback == null then
        attachment.messageCallback = attachment.incoming.connectionEstablished(NioConnection(attachment.layer, framing))

  private def deliver(attachment: ReceiveAttachment, message: Array[Byte]): Unit = {
    val callback = attachment.messageCallback.nn
    accepCallbackExecutor.execute(() => callback.succeed(ByteBufferMessageBuffer(message)))
  }

  /** Returns the next protocol state. */
  private def handleWebsocketFrame(
      fin: Boolean,
      websocketFrame: WebsocketFrame,
      attachment: ReceiveAttachment,
      key: SelectionKey,
      fragments: Vector[Array[Byte]]
  ): ProtocolState = {

    def data(bytes: Array[Byte]): ProtocolState =
      if fin then
          deliver(attachment, if fragments.isEmpty then bytes else (fragments :+ bytes).toArray.flatten)
          ProtocolState.WebSocket(Vector.empty)
      else ProtocolState.WebSocket(fragments :+ bytes)

    websocketFrame match {
      case BinaryFrame(bytes)        => data(bytes)
      case TextFrame(text)           => data(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      case ReservedFrame(0x0, bytes) => data(bytes)
      case PingFrame(bytes)          =>
        attachment.layer.write(Array(ByteBuffer.wrap(WebsocketProtocol.encodePongFrame(bytes))))
        ProtocolState.WebSocket(fragments)
      case PongFrame(_) | ReservedFrame(_, _) =>
        ProtocolState.WebSocket(fragments)
      case CloseFrame(_, _) =>
        try attachment.layer.write(Array(ByteBuffer.wrap(WebsocketProtocol.encodeCloseFrame())))
        catch case NonFatal(_) => ()
        attachment.messageCallback.nn.fail(NoMoreDataException("websocket closed"))
        attachment.layer.close()
        key.cancel()
        ProtocolState.Closed
    }
  }

  private def failKey(key: SelectionKey, ex: Exception): Unit = {
    val channel = key.channel()
    try channel.close()
    catch case NonFatal(_) => ()
    key.cancel()
    key.attachment() match {
      case attachment: ReceiveAttachment =>
        attachment.layer.failed(ex)
        val callback = attachment.messageCallback
        if callback != null then callback.fail(ex)
      case _ => ()
    }
  }
}
