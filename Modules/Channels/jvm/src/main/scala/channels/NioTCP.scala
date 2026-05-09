package channels

import channels.NioTCP.*
import channels.WebsocketProtocol.WebsocketHeader
import de.rmgk.delay.{Async, Callback, Sync}
import replication.BroadcastIO

import java.net.{InetSocketAddress, SocketAddress, StandardProtocolFamily, StandardSocketOptions, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext
import scala.util.Try
import scala.util.control.NonFatal

object NioTCP {
  case class AcceptAttachment(
      callback: Callback[Connection],
      incoming: Receive,
  )

  case class WebSocketState(
      fragments: Vector[Array[Byte]] = Vector.empty,
      fragmentOpcode: Option[Int] = None,
      pendingHeader: Option[WebsocketHeader] = None,
  )

  /** Per-socket protocol mode.
    *
    * New inbound sockets start in [[Init]] so we can sniff the first bytes.
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
    */
  case class ReceiveAttachment(
      connectCallback: Callback[Connection] | Null,
      incoming: Receive,
      protocol: ProtocolState = ProtocolState.Init,
      messageCallback: Callback[MessageBuffer] | Null = null,
      primary: ByteBuffer = ByteBuffer.allocate(1024),
      secondary: ByteBuffer | Null = null,
  )

  class EndOfChannelException(msg: String) extends Exception(msg)

  extension [T](broadcast: BroadcastIO[T])
      def serveNioLoop(): Unit = {
        val niotcp = new NioTCP()
        broadcast.addBinaryConnection(niotcp.listen(niotcp.defaultServerSocketChannel(new InetSocketAddress(0))))
        niotcp.loopSelection(broadcast.globalAbort)
      }

}

object ConcurrencyHelper {
  def makeExecutionContext(singleThreadExecutor: Boolean) = {
    if singleThreadExecutor then
        val singleThreadExecutor: ExecutorService = Executors.newSingleThreadExecutor { r =>
          val thread = new Thread(r)
          thread.setDaemon(true)
          thread
        }

        ExecutionContext.fromExecutorService(singleThreadExecutor)
    else
        BroadcastIO.executeImmediately
  }

  def makePooledExecutor() = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
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
class NioTCP(accepCallbackExecutor: ExecutionContext = BroadcastIO.executeImmediately) {
  inline val compression: false = false

  val selector: Selector = Selector.open()

  def loopSelection(abort: Abort): Unit = {
    while !abort.closeRequest do
        selector.select()
        runSelection()
  }

  def runSelection(): Unit = {

    selector.selectedKeys().forEach { key =>
      try {
        if key.isValid && key.isReadable then
            val clientChannel = key.channel().asInstanceOf[SocketChannel]
            val attachment    = key.attachment().asInstanceOf[ReceiveAttachment]
            handleReadable(key, clientChannel, attachment)
        else if key.isValid && key.isAcceptable then
            val serverChannel = key.channel().asInstanceOf[ServerSocketChannel]
            val attachment    = key.attachment().asInstanceOf[AcceptAttachment]
            val clientChannel = serverChannel.accept()
            if clientChannel != null then {
              configureChannel(clientChannel)
              clientChannel.register(
                selector,
                SelectionKey.OP_READ,
                ReceiveAttachment(attachment.callback, attachment.incoming)
              )
              selector.wakeup()
              ()
            }
      } catch {
        case ex: Exception =>
          failKey(key, ex)
      }
    }

    selector.selectedKeys().clear()
  }

  private def socketConnectInfo(address: SocketAddress): Option[ConnectionDescriptor.Tcp] =
    address match
        case isa: InetSocketAddress => Some(ConnectionDescriptor.Tcp(isa.getHostString, isa.getPort))
        case _                      => None

  class NioTCPConnection(clientChannel: SocketChannel) extends Connection {

    override val info: ConnectionInfo = ConnectionInfo(
      local = Try(socketConnectInfo(clientChannel.getLocalAddress)).getOrElse(None),
      remote = Try(socketConnectInfo(clientChannel.getRemoteAddress)).getOrElse(None),
      details = Map("type" -> "niotcp")
    )

    override def send(message: MessageBuffer): Async[Any, Unit] = Sync {
      val bytes         = message.asArray
      val messageLength = bytes.length

      val sizeBuffer = ByteBuffer.allocate(4)
      sizeBuffer.putInt(messageLength)
      sizeBuffer.flip()

      writeFully(clientChannel, Array(sizeBuffer, ByteBuffer.wrap(bytes)))
      ()
    }

    override def close(): Unit = clientChannel.close()
  }

  class WebSocketConnection(clientChannel: SocketChannel) extends Connection {

    override val info: ConnectionInfo = ConnectionInfo(
      local = Try(socketConnectInfo(clientChannel.getLocalAddress)).getOrElse(None),
      remote = Try(socketConnectInfo(clientChannel.getRemoteAddress)).getOrElse(None),
      details = Map("type" -> "websocket")
    )

    override def send(message: MessageBuffer): Async[Any, Unit] = Sync {
      val payload = message.asArray
      val frame   = WebsocketProtocol.encodeBinaryFrame(payload)
      writeFully(clientChannel, Array(ByteBuffer.wrap(frame)))
      ()
    }

    override def close(): Unit = {
      try writeFully(clientChannel, Array(ByteBuffer.wrap(WebsocketProtocol.encodeCloseFrame())))
      catch case _: Throwable => ()
      clientChannel.close()
    }
  }

  def connect(
      bindsocket: () => SocketChannel,
  ): LatentConnection =
    new LatentConnection {
      override def prepare(incoming: Receive): Async[Any, Connection] =
        Async.fromCallback {
          try
              Async.handler.succeed {
                handleConnectTo(bindsocket(), incoming)
              }
          catch case NonFatal(exception) => Async.handler.fail(exception)
        }
    }

  def handleConnectTo(
      clientChannel: SocketChannel,
      incoming: Receive,
  ): NioTCPConnection = {

    configureChannel(clientChannel)

    val conn     = NioTCPConnection(clientChannel)
    val callback = incoming.messageHandler(conn)
    clientChannel.register(
      selector,
      SelectionKey.OP_READ,
      ReceiveAttachment(null, incoming, ProtocolState.Init, callback)
    )
    selector.wakeup()

    conn
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

  def listen(
      bindsocket: () => ServerSocketChannel,
  ): LatentConnection =
    new LatentConnection {
      override def prepare(incoming: Receive): Async[Abort, Connection] =
        Async.fromCallback { abort ?=>
          try {
            val serverChannel: ServerSocketChannel = bindsocket()

            val callback = Async.handler[Connection]
            serverChannel.register(selector, SelectionKey.OP_ACCEPT, AcceptAttachment(callback, incoming))
            selector.wakeup()
            ()
          } catch
              case NonFatal(ex) => Async.handler.fail(ex)

        }
    }

  private def handleReadable(
      key: SelectionKey,
      clientChannel: SocketChannel,
      attachment: ReceiveAttachment
  ): Unit = {
    val target =
      if attachment.secondary == null
      then Array(attachment.primary)
      else Array(attachment.primary, attachment.secondary)
    val read = clientChannel.read(target)

    attachment.primary.flip()
    attachment.protocol match {
      case ProtocolState.WebSocket(ws) if ws.pendingHeader.isDefined && attachment.secondary != null =>
        attachment.secondary.flip()
      case _ => ()
    }
    val initialBytes = totalAvailable(attachment)

    val next = attachment.protocol match {
      case ProtocolState.Init               => handleInitial(clientChannel, attachment)
      case ProtocolState.Plain(len)         => handlePlain(len, attachment)
      case ProtocolState.WebSocketHandshake => handleWebSocketHandshake(clientChannel, attachment)
      case ProtocolState.WebSocket(ws)      => ws.pendingHeader match {
          case None    => handleWebsocketHeader(attachment)
          case Some(_) => handleWebsocketBody(attachment, clientChannel, key, ws)
        }
    }
    key.attach(next)
    if read == -1 then throw NoMoreDataException("remaining channel is empty")
    val bufferedAfter = bufferedBytes(next)
    val recurse       = bufferedAfter > 0 && bufferedAfter < initialBytes
    if recurse then handleReadable(key, clientChannel, next)
    ()
  }

  private def handleInitial(clientChannel: SocketChannel, attachment: ReceiveAttachment) = {
    if attachment.primary.remaining() < 4 then
        attachment.primary.compact()
        attachment
    else
        val len = attachment.primary.getInt
        if len == WebsocketProtocol.handshakePrefix then
            handleWebSocketHandshake(clientChannel, attachment.copy(protocol = ProtocolState.WebSocketHandshake))
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
                  val conn     = NioTCPConnection(clientChannel)
                  val callback = upat.incoming.messageHandler(conn)
                  if upat.connectCallback != null then upat.connectCallback.succeed(conn)
                  upat.copy(messageCallback = callback)
            handlePlain(
              len,
              withCallback
            )
  }

  def totalAvailable(attachment: ReceiveAttachment): Int =
    if attachment.secondary == null then attachment.primary.remaining()
    else attachment.primary.remaining() + attachment.secondary.remaining()

  def bufferedBytes(attachment: ReceiveAttachment): Int =
    if attachment.secondary == null then attachment.primary.position()
    else attachment.primary.position() + attachment.secondary.position()

  private def handlePlain(len: Int, attachment: ReceiveAttachment): ReceiveAttachment = {
    val callback = attachment.messageCallback.nn

    attachment.primary.flip()
    if attachment.secondary != null then { attachment.secondary.flip(); () }

    grabPayload(len, attachment) match {
      case Some(buffer) =>

        accepCallbackExecutor.execute { () =>
          callback.succeed(ArrayMessageBuffer(buffer))
        }

        attachment.primary.compact()
        attachment.copy(secondary = null, protocol = ProtocolState.Init)
      case None =>
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
      clientChannel: SocketChannel,
      attachment: ReceiveAttachment
  ): ReceiveAttachment = {
    attachment.primary.mark()

    WebsocketProtocol.tryParseHandshake(attachment.primary) match {
      case None =>
        attachment.primary.reset()
        attachment.primary.compact()
        attachment
      case Some(request) =>
        writeFully(clientChannel, Array(ByteBuffer.wrap(WebsocketProtocol.handshakeResponse(request))))
        val connected =
          if attachment.messageCallback != null then attachment
          else
              val conn     = WebSocketConnection(clientChannel)
              val callback = attachment.incoming.messageHandler(conn)
              if attachment.connectCallback != null then attachment.connectCallback.succeed(conn)
              attachment.copy(messageCallback = callback)

        attachment.primary.compact()

        connected.copy(
          protocol = ProtocolState.WebSocket(WebSocketState()),
        )
    }
  }

  private def handleWebsocketBody(
      attachment: ReceiveAttachment,
      clientChannel: SocketChannel,
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
        handleWebsocketMessage(header, frame, attachment, clientChannel, key, ws).copy(
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
      clientChannel: SocketChannel,
      key: SelectionKey | Null,
      ws: WebSocketState
  ): ReceiveAttachment = {

    val callback       = attachment.messageCallback.nn
    var fragments      = ws.fragments
    var fragmentOpcode = ws.fragmentOpcode

    val fin = header.fin

    websocketFrame match {
      case BinaryFrame(data) =>
        if fin then accepCallbackExecutor.execute(() => callback.succeed(ArrayMessageBuffer(data)))
        else {
          fragments = fragments :+ data
          fragmentOpcode = Some(0x2)
        }

      case TextFrame(text) =>
        val data = text.getBytes(java.nio.charset.StandardCharsets.UTF_8)
        if fin then accepCallbackExecutor.execute(() => callback.succeed(ArrayMessageBuffer(data)))
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
          accepCallbackExecutor.execute(() => callback.succeed(ArrayMessageBuffer(complete)))
        }

      case PingFrame(data) =>
        writeFully(clientChannel, Array(ByteBuffer.wrap(WebsocketProtocol.encodePongFrame(data))))

      case PongFrame(_) =>
        ()

      case CloseFrame(_, _) =>
        try writeFully(clientChannel, Array(ByteBuffer.wrap(WebsocketProtocol.encodeCloseFrame())))
        catch case _: Throwable => ()
        callback.fail(NoMoreDataException("websocket closed"))
        clientChannel.close()
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

  private def writeFully(clientChannel: SocketChannel, buffers: Array[ByteBuffer]): Unit = {
    while buffers.exists(_.hasRemaining()) do {
      clientChannel.write(buffers)
      ()
    }
  }

  private def failKey(key: SelectionKey, ex: Exception): Unit = {
    val channel = key.channel()
    try channel.close()
    catch case _: Throwable => ()
    key.cancel()
    key.attachment() match {
      case attachment: ReceiveAttachment =>
        if attachment.messageCallback != null then attachment.messageCallback.fail(ex)
        else if attachment.connectCallback != null then attachment.connectCallback.fail(ex)
        else ()
      case _ => ()
    }
  }
}
