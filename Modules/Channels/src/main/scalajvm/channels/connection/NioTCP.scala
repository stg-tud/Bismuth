package channels
package connection

import channels.connection.NioTCP.*
import channels.connection.WebsocketProtocol.WebsocketHeader
import channels.connection.{Abort, ByteBufferMessageBuffer, Connection, ConnectionDescriptor, ConnectionInfo, Connector, Listener, MessageBuffer, NoMoreDataException, Receive}
import de.rmgk.delay.{Async, Callback, Sync}

import java.net.{InetSocketAddress, SocketAddress, StandardProtocolFamily, StandardSocketOptions, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object NioTCP {

  /** `tls` configures TLS for all accepted connections, if defined. */
  case class AcceptAttachment(
      incoming: Receive,
      tls: Option[NioTls] = None,
  )

  case class WebSocketState(
      fragments: Vector[Array[Byte]] = Vector.empty,
      fragmentOpcode: Option[Int] = None,
      pendingHeader: Option[WebsocketHeader] = None,
  )

  /** Per-socket protocol mode.
    *
    * New inbound sockets start in `Init` so we can sniff the first bytes.
    * After that, each connection independently continues either as normal length-prefixed NioTCP traffic,
    * or as a websocket handshake followed by websocket frames.
    */
  enum ProtocolState {
    case Init
    case Plain(len: Int)
    case WebSocketHandshake
    case WebSocket(state: WebSocketState)
  }

  /** Per-connection attachment stored on the selector key.
    *
    * A single attachment type is used for both plain NioTCP and websocket traffic, so the event loop can keep one
    * readable-path and only switch behavior based on [[protocol]].
    *
    * Different fields matter in different phases:
    *   - `connectCallback` is used only for inbound accepted sockets, to publish the established connection once the
    *     protocol is known
    *   - `messageCallback` is populated once the connection has been initialized
    *   - `buffered` stores unread bytes across selector turns for either protocol
    *   - `WebSocketState` in the protocol holds `fragments` and `fragmentOpcode` for fragmented websocket messages
    *   - `layer` is the byte stream all protocols read from and write to (the raw socket, or TLS on top of it)
    *   - `onConnected` is set for outgoing connections whose layer is not yet ready (TLS handshake), it completes the
    *     connector once the layer is ready, or failed
    */
  case class ReceiveAttachment(
      incoming: Receive,
      layer: NioSocketLayer,
      protocol: ProtocolState = ProtocolState.Init,
      messageCallback: Callback[MessageBuffer] | Null = null,
      onConnected: (Try[Connection] => Unit) | Null = null,
      primary: ByteBuffer = ByteBuffer.allocate(1024),
      secondary: ByteBuffer | Null = null,
  )

  class EndOfChannelException(msg: String) extends Exception(msg)

  extension (broadcast: MessagingIO[?])
      def serveNioLoop(): Unit = {
        val niotcp = new NioTCP()
        broadcast.addServerConnection(niotcp.listen(niotcp.defaultServerSocketChannel(new InetSocketAddress(0))))
        niotcp.loopSelection(broadcast.globalAbort)
      }

}

object ConcurrencyHelper {
  def makeExecutionContext(singleThreadExecutor: Boolean): ExecutionContext =
    if singleThreadExecutor then
        val singleThreadExecutor: ExecutorService = Executors.newSingleThreadExecutor { r =>
          val thread = new Thread(r)
          thread.setDaemon(true)
          thread
        }

        ExecutionContext.fromExecutorService(singleThreadExecutor)
    else
        MessagingIO.executeImmediately

  def makePooledExecutor(): ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
}

/** Selector-based TCP transport.
  *
  * In addition to the original length-prefixed NioTCP framing, this implementation can accept websocket clients on the
  * same listening socket. Each accepted connection is sniffed once, then handled either as plain NioTCP or as
  * websocket traffic. Different connections may use different protocols concurrently.
  *
  * [[loopSelection]] and [[runSelection]] should not be called from multiple threads at the same time.
  * Only one thread should send on a single connection at the same time.
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
        if key.isValid && key.isReadable then
            handleReadable(key, key.attachment().asInstanceOf[ReceiveAttachment])
        else if key.isValid && key.isAcceptable then
            val serverChannel = key.channel().asInstanceOf[ServerSocketChannel]
            val attachment    = key.attachment().asInstanceOf[AcceptAttachment]
            val clientChannel = serverChannel.accept()
            if clientChannel != null then {
              configureChannel(clientChannel)
              val layer = attachment.tls match
                  case None      => NioSocketLayer.Plain(clientChannel)
                  case Some(tls) => NioSocketLayer.Tls(clientChannel, tls.createEngine(clientMode = false), tls)
              clientChannel.register(selector, SelectionKey.OP_READ, ReceiveAttachment(attachment.incoming, layer))
              selector.wakeup()
              ()
            }
      catch {
        case ex: Exception =>
          failKey(key, ex)
      }
    }

    selector.selectedKeys().clear()
  }

  class NioTCPConnection(layer: NioSocketLayer) extends Connection {

    override val info: ConnectionInfo = layer.connectionInfo("type" -> "niotcp")

    override def send(message: MessageBuffer): Async[Any, Unit] = Sync {
      val bytes         = message.asByteBuffer
      val messageLength = bytes.remaining()

      val sizeBuffer = ByteBuffer.allocate(4)
      sizeBuffer.putInt(messageLength)
      sizeBuffer.flip()

      layer.write(Array(sizeBuffer, bytes))
    }

    override def close(): Unit = layer.close()
  }

  class WebSocketConnection(layer: NioSocketLayer) extends Connection {

    override val info: ConnectionInfo = layer.connectionInfo("type" -> "websocket")

    override def send(message: MessageBuffer): Async[Any, Unit] = Sync {
      val payload =
          val bb = message.asByteBuffer
          bb.array()
          val arr = new Array[Byte](bb.remaining())
          bb.get(arr)
          arr
      val frame = WebsocketProtocol.encodeBinaryFrame(payload)
      layer.write(Array(ByteBuffer.wrap(frame)))
    }

    override def close(): Unit = {
      try layer.write(Array(ByteBuffer.wrap(WebsocketProtocol.encodeCloseFrame())))
      catch case _: Throwable => ()
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
  ): NioTCPConnection = {

    configureChannel(clientChannel)

    val layer    = NioSocketLayer.Plain(clientChannel)
    val conn     = NioTCPConnection(layer)
    val callback = incoming.connectionEstablished(conn)
    clientChannel.register(
      selector,
      SelectionKey.OP_READ,
      ReceiveAttachment(incoming, layer, ProtocolState.Init, callback)
    )
    selector.wakeup()

    conn
  }

  /** Like [[connect]], but runs TLS configured by `tls` on the socket.
    * Completes (and reports the connection to the receiver) once the handshake is done.
    */
  def connectTls(bindsocket: () => SocketChannel, tls: NioTls): Connector =
    new Connector {
      override def connect(incoming: Receive): Async[Any, Connection] =
        Async.fromCallback {
          val handler = Async.handler
          try
              val clientChannel = bindsocket()
              configureChannel(clientChannel)
              val layer = NioSocketLayer.Tls(clientChannel, tls.createEngine(clientMode = true), tls)
              val onConnected: Try[Connection] => Unit = {
                case Success(conn) => handler.succeed(conn)
                case Failure(ex)   => handler.fail(ex)
              }
              clientChannel.register(
                selector,
                SelectionKey.OP_READ,
                ReceiveAttachment(incoming, layer, onConnected = onConnected)
              )
              selector.wakeup()
              ()
          catch case NonFatal(exception) => handler.fail(exception)
        }
    }

  def defaultSocketChannel(socketAddress: SocketAddress): () => SocketChannel = () => {
    val pf = socketAddress match
        case _: UnixDomainSocketAddress => StandardProtocolFamily.UNIX
        case _                          => StandardProtocolFamily.INET
    val channel = SocketChannel.open(pf)
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

  def defaultServerSocketChannel(socketAddress: SocketAddress): () => ServerSocketChannel = () => {
    val pf = socketAddress match
        case _: UnixDomainSocketAddress => StandardProtocolFamily.UNIX
        case _                          => StandardProtocolFamily.INET
    val socket = ServerSocketChannel.open(pf)
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
  ): Listener = listen(bindsocket, None)

  /** Like [[listen]], but runs TLS configured by `tls` on every accepted socket. Only IP sockets are supported. */
  def listenTls(bindsocket: () => ServerSocketChannel, tls: NioTls): Listener = listen(bindsocket, Some(tls))

  private def listen(
      bindsocket: () => ServerSocketChannel,
      tls: Option[NioTls],
  ): Listener =
    new Listener {
      override def listen(incoming: Receive): Async[Abort, ConnectionDescriptor] =
        Async.fromCallback { abort ?=>
          try {
            val serverChannel: ServerSocketChannel = bindsocket()

            val descriptor = (serverChannel.getLocalAddress, tls) match {
              case (isa: InetSocketAddress, None) =>
                Some(ConnectionDescriptor.TcpWebSocket(isa.getHostString, isa.getPort))
              case (isa: InetSocketAddress, Some(_)) =>
                Some(ConnectionDescriptor.TcpTls(isa.getHostString, isa.getPort))
              case (uda: UnixDomainSocketAddress, None) =>
                Some(ConnectionDescriptor.Unix(uda.getPath.toString))
              case _ => None
            }

            descriptor match
                case Some(descriptor) =>
                  serverChannel.register(selector, SelectionKey.OP_ACCEPT, AcceptAttachment(incoming, tls))
                  selector.wakeup()
                  Async.handler.succeed(descriptor)
                case None =>
                  Async.handler.fail(IllegalStateException(s"unsupported socket address for $tls"))

            ()
          } catch
              case NonFatal(ex) => Async.handler.fail(ex)

        }
    }

  private def handleReadable(
      key: SelectionKey,
      readAttachment: ReceiveAttachment
  ): Unit = {
    val target =
      if readAttachment.secondary == null
      then Array(readAttachment.primary)
      else Array(readAttachment.primary, readAttachment.secondary)
    val read = readAttachment.layer.read(target)

    val attachment =
      if readAttachment.onConnected != null && readAttachment.layer.ready then
          val conn     = NioTCPConnection(readAttachment.layer)
          val callback = readAttachment.incoming.connectionEstablished(conn)
          readAttachment.onConnected(Success(conn))
          readAttachment.copy(messageCallback = callback, onConnected = null)
      else readAttachment

    attachment.primary.flip()
    attachment.protocol match {
      case ProtocolState.WebSocket(ws) if ws.pendingHeader.isDefined && attachment.secondary != null =>
        attachment.secondary.flip()
      case ProtocolState.Plain(_) if attachment.secondary != null =>
        attachment.secondary.flip()
      case _ => ()
    }
    val initialBytes = totalAvailable(attachment)

    val next = attachment.protocol match {
      case ProtocolState.Init               => handleInitial(attachment)
      case ProtocolState.Plain(len)         => handlePlain(len, attachment)
      case ProtocolState.WebSocketHandshake => handleWebSocketHandshake(attachment)
      case ProtocolState.WebSocket(ws)      => ws.pendingHeader match {
          case None    => handleWebsocketHeader(attachment)
          case Some(_) => handleWebsocketBody(attachment, key, ws)
        }
    }
    key.attach(next)
    if read == -1 then throw NoMoreDataException("remaining channel is empty")
    val bufferedAfter = bufferedBytes(next)
    val recurse       =
      (bufferedAfter > 0 && bufferedAfter < initialBytes) || (read > 0 && attachment.layer.hasBufferedInput)
    if recurse then handleReadable(key, next)
    ()
  }

  private def handleInitial(attachment: ReceiveAttachment) =
    if attachment.primary.remaining() < 4 then
        attachment.primary.compact()
        attachment
    else
        val len = attachment.primary.getInt
        if len == WebsocketProtocol.handshakePrefix then
            handleWebSocketHandshake(attachment.copy(protocol = ProtocolState.WebSocketHandshake))
        else
            require(len < MessageBuffer.maxPayloadSize, "message too large")
            attachment.primary.compact()
            val upat =
              if len > 1024
              then attachment.copy(secondary = ByteBuffer.allocate(len - 1024), protocol = ProtocolState.Plain(len))
              else attachment.copy(protocol = ProtocolState.Plain(len))

            val withCallback =
              if upat.messageCallback != null then upat
              else
                  val conn     = NioTCPConnection(attachment.layer)
                  val callback = upat.incoming.connectionEstablished(conn)
                  upat.copy(messageCallback = callback)
            // handlePlain expects buffers in read mode
            withCallback.primary.flip()
            if withCallback.secondary != null then withCallback.secondary.flip(): Unit
            handlePlain(
              len,
              withCallback
            )

  def totalAvailable(attachment: ReceiveAttachment): Int =
    if attachment.secondary == null then attachment.primary.remaining()
    else attachment.primary.remaining() + attachment.secondary.remaining()

  def bufferedBytes(attachment: ReceiveAttachment): Int =
    if attachment.secondary == null then attachment.primary.position()
    else attachment.primary.position() + attachment.secondary.position()

  /** Expects the buffers in read mode, leaves them in write mode. */
  private def handlePlain(len: Int, attachment: ReceiveAttachment): ReceiveAttachment = {
    val callback = attachment.messageCallback.nn

    grabPayload(len, attachment) match {
      case Some(buffer) =>

        accepCallbackExecutor.execute { () =>
          callback.succeed(ByteBufferMessageBuffer(buffer))
        }

        attachment.primary.compact()
        attachment.copy(secondary = null, protocol = ProtocolState.Init)
      case None =>
        attachment.primary.compact()
        if attachment.secondary != null then attachment.secondary.compact(): Unit
        attachment
    }
  }

  private def grabPayload(len: Int, attachment: ReceiveAttachment): Option[Array[Byte]] = {
    val available = totalAvailable(attachment)

    if available >= len then
        val buffer      = new Array[Byte](len)
        val fromPrimary = math.min(len, attachment.primary.remaining())
        attachment.primary.get(buffer, 0, fromPrimary)
        if attachment.secondary != null && fromPrimary < len then
            attachment.secondary.get(buffer, fromPrimary, len - fromPrimary)
            assert(attachment.secondary.remaining() == 0, "secondary buffer should be empty")
            ()
        Some(buffer)
    else None
  }

  private def handleWebSocketHandshake(
      attachment: ReceiveAttachment
  ): ReceiveAttachment = {
    attachment.primary.mark()

    WebsocketProtocol.tryParseHandshake(attachment.primary) match {
      case None =>
        attachment.primary.reset()
        attachment.primary.compact()
        attachment
      case Some(request) =>
        attachment.layer.write(Array(ByteBuffer.wrap(WebsocketProtocol.handshakeResponse(request))))
        val connected =
          if attachment.messageCallback != null then attachment
          else
              val conn     = WebSocketConnection(attachment.layer)
              val callback = attachment.incoming.connectionEstablished(conn)
              attachment.copy(messageCallback = callback)

        attachment.primary.compact()

        connected.copy(
          protocol = ProtocolState.WebSocket(WebSocketState()),
        )
    }
  }

  private def handleWebsocketBody(
      attachment: ReceiveAttachment,
      key: SelectionKey | Null,
      ws: WebSocketState
  ) = {
    val header = ws.pendingHeader.get
    attachment.primary.mark()
    grabPayload(header.len, attachment) match {
      case Some(value) =>
        val frame = WebsocketProtocol.tryParsePayload(header, value)
        assert(
          attachment.secondary == null || !attachment.secondary.hasRemaining,
          "secondary buffer should be fully consumed"
        )
        attachment.primary.compact()
        handleWebsocketMessage(header, frame, attachment, key, ws).copy(
          secondary = null
        )
      case None =>
        attachment.primary.reset()
        attachment.primary.compact()
        if attachment.secondary != null then
            attachment.secondary.compact()
            ()
        attachment
    }
  }

  private def handleWebsocketHeader(attachment: ReceiveAttachment) = {
    attachment.primary.mark()
    WebsocketProtocol.tryParseHeader(attachment.primary) match {
      case None =>
        attachment.primary.reset()
        attachment.primary.compact()
        attachment
      case Some(value) =>
        attachment.primary.compact()
        require(value.len < MessageBuffer.maxPayloadSize, "message too large")
        val ws = attachment.protocol match {
          case ProtocolState.WebSocket(ws) => ws.copy(pendingHeader = Some(value))
          case _                           => WebSocketState(pendingHeader = Some(value))
        }
        if value.len > 1024
        then
            attachment.copy(
              protocol = ProtocolState.WebSocket(ws),
              secondary = ByteBuffer.allocate(value.len - 1024)
            )
        else attachment.copy(protocol = ProtocolState.WebSocket(ws))
    }
  }

  def handleWebsocketMessage(
      header: WebsocketHeader,
      websocketFrame: WebsocketFrame,
      attachment: ReceiveAttachment,
      key: SelectionKey | Null,
      ws: WebSocketState
  ): ReceiveAttachment = {

    val callback       = attachment.messageCallback.nn
    var fragments      = ws.fragments
    var fragmentOpcode = ws.fragmentOpcode

    val fin = header.fin

    websocketFrame match {
      case BinaryFrame(data) =>
        if fin then accepCallbackExecutor.execute(() => callback.succeed(ByteBufferMessageBuffer(data)))
        else {
          fragments = fragments :+ data
          fragmentOpcode = Some(0x2)
        }

      case TextFrame(text) =>
        val data = text.getBytes(java.nio.charset.StandardCharsets.UTF_8)
        if fin then accepCallbackExecutor.execute(() => callback.succeed(ByteBufferMessageBuffer(data)))
        else {
          fragments = fragments :+ data
          fragmentOpcode = Some(0x1)
        }

      case ReservedFrame(0x0, data) =>
        fragments = fragments :+ data
        if fin then {
          val complete = fragments.foldLeft(Array.emptyByteArray)(_ ++ _)
          fragments = Vector.empty
          fragmentOpcode = None
          accepCallbackExecutor.execute(() => callback.succeed(ByteBufferMessageBuffer(complete)))
        }

      case PingFrame(data) =>
        attachment.layer.write(Array(ByteBuffer.wrap(WebsocketProtocol.encodePongFrame(data))))

      case PongFrame(_) =>
        ()

      case CloseFrame(_, _) =>
        try attachment.layer.write(Array(ByteBuffer.wrap(WebsocketProtocol.encodeCloseFrame())))
        catch case _: Throwable => ()
        callback.fail(NoMoreDataException("websocket closed"))
        attachment.layer.close()
        if key != null then {
          key.cancel()
          ()
        }

      case ReservedFrame(_, _) =>
        ()
    }

    val updatedWs = ws.copy(fragments = fragments, fragmentOpcode = fragmentOpcode, pendingHeader = None)
    attachment.copy(protocol = ProtocolState.WebSocket(updatedWs))
  }

  private def failKey(key: SelectionKey, ex: Exception): Unit = {
    val channel = key.channel()
    try channel.close()
    catch case _: Throwable => ()
    key.cancel()
    key.attachment() match {
      case attachment: ReceiveAttachment =>
        if attachment.messageCallback != null then attachment.messageCallback.fail(ex)
        else if attachment.onConnected != null then attachment.onConnected(Failure(ex))
        else ()
      case _ => ()
    }
  }
}
