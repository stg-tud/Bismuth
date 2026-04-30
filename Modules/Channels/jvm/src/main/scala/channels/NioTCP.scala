package channels

import channels.NioTCP.*
import de.rmgk.delay.{Async, Callback, Sync}
import replication.{Compression, PlumtreeDissemination}

import java.io.ByteArrayOutputStream
import java.net.{SocketAddress, StandardProtocolFamily, StandardSocketOptions, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import java.util.concurrent.{ExecutorService, Executors}
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.ExecutionContext
import scala.util.Try
import scala.util.control.NonFatal

class ChannelTrafficReporter {
  val receivedBytes: AtomicLong = AtomicLong()
  val sentBytes: AtomicLong     = AtomicLong()
  val receivedCount: AtomicLong = AtomicLong()
  val sentCount: AtomicLong     = AtomicLong()
  val maxReceived: AtomicLong   = AtomicLong()
  val maxSent: AtomicLong       = AtomicLong()

  def reset(): Unit = {
    receivedBytes.set(0)
    sentBytes.set(0)
    receivedCount.set(0)
    sentCount.set(0)
    maxSent.set(0)
    maxReceived.set(0)
  }

  def report(): String =
    s"received:\n  ${receivedCount.get()} messages\n  ${maxReceived.get()} max\n  ${receivedBytes.get()} bytes\nsent:\n  ${sentCount.get()} messages\n  ${maxSent.get()} max\n  ${sentBytes.get()} bytes"
}
object ChannelTrafficReporter {
  extension (reporter: ChannelTrafficReporter | Null) {
    inline def received(size: Long): Unit = if reporter != null then
        reporter.maxReceived.accumulateAndGet(size, Math.max)
        reporter.receivedBytes.addAndGet(size)
        reporter.receivedCount.incrementAndGet()
        ()
    inline def send(size: Long): Unit = if reporter != null then
        reporter.maxSent.accumulateAndGet(size, Math.max)
        reporter.sentBytes.addAndGet(size)
        reporter.sentCount.incrementAndGet()
        ()
  }
}

object NioTCP {
  case class AcceptAttachment(
      callback: Callback[Connection[MessageBuffer]],
      incoming: Receive[MessageBuffer],
  )

  /** Per-socket protocol mode.
    *
    * New inbound sockets start in [[Inspecting]] so we can sniff the first bytes.
    * After that, each connection independently continues either as normal length-prefixed NioTCP traffic,
    * or as a websocket handshake followed by websocket frames.
    */
  enum ProtocolState {
    case Inspecting, Plain, WebSocketHandshake, WebSocket
  }

  /** Per-connection attachment stored on the selector key.
    *
    * A single attachment type is used for both plain NioTCP and websocket traffic, so the event loop can keep one
    * readable-path and only switch behavior based on [[protocol]].
    *
    * Different fields matter in different phases:
    *   - `connectCallback` is used only for inbound accepted sockets, to publish the established connection once the
    *     protocol is known
    *   - `connection` and `messageCallback` are populated once the connection has been initialized
    *   - `buffered` stores unread bytes across selector turns for either protocol
    *   - `fragments` and `fragmentOpcode` are only used for fragmented websocket messages
    */
  case class ReceiveAttachment(
      connectCallback: Callback[Connection[MessageBuffer]] | Null,
      incoming: Receive[MessageBuffer],
      protocol: ProtocolState = ProtocolState.Inspecting,
      connection: Connection[MessageBuffer] | Null = null,
      messageCallback: Callback[MessageBuffer] | Null = null,
      buffered: Array[Byte] = Array.emptyByteArray,
      fragments: Vector[Array[Byte]] = Vector.empty,
      fragmentOpcode: Option[Int] = None,
  )

  class EndOfChannelException(msg: String) extends Exception(msg)
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
        PlumtreeDissemination.executeImmediately
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
class NioTCP(pool: ExecutionContext, reporter: ChannelTrafficReporter | Null = null) {
  inline val compression: false = false

  private val sniffLength    = 4
  private val readBufferSize = 8192

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

  class NioTCPConnection(clientChannel: SocketChannel) extends Connection[MessageBuffer] {

    override val info: ConnectionInfo = ConnectionInfo(
      "type"          -> "niotcp",
      "remoteAddress" -> Try { clientChannel.getRemoteAddress.toString }.recover(_.getMessage).get,
      "localAddress"  -> Try { clientChannel.getLocalAddress.toString }.recover(_.getMessage).get
    )

    override def send(message: MessageBuffer): Async[Any, Unit] = Sync {
      val bytes           = message.asArray
      val compressedBytes = if compression then Compression.compress(bytes) else bytes
      val messageLength   = compressedBytes.length

      val sizeBuffer = ByteBuffer.allocate(4)
      sizeBuffer.putInt(messageLength)
      sizeBuffer.flip()

      writeFully(clientChannel, Array(sizeBuffer, ByteBuffer.wrap(compressedBytes)))
      reporter.send(messageLength + 4)
      ()
    }

    override def close(): Unit = clientChannel.close()
  }

  class WebSocketConnection(clientChannel: SocketChannel) extends Connection[MessageBuffer] {

    override val info: ConnectionInfo = ConnectionInfo(
      "type"          -> "websocket",
      "remoteAddress" -> Try { clientChannel.getRemoteAddress.toString }.recover(_.getMessage).get,
      "localAddress"  -> Try { clientChannel.getLocalAddress.toString }.recover(_.getMessage).get
    )

    override def send(message: MessageBuffer): Async[Any, Unit] = Sync {
      val frame = WebsocketProtocol.encodeBinaryFrame(message.asArray)
      writeFully(clientChannel, Array(ByteBuffer.wrap(frame)))
      reporter.send(frame.length)
      ()
    }

    override def close(): Unit = {
      try writeFully(clientChannel, Array(ByteBuffer.wrap(WebsocketProtocol.encodeCloseFrame())))
      catch case _: Throwable => ()
      clientChannel.close()
    }
  }

  def handleConnection(
      clientChannel: SocketChannel,
      incoming: Receive[MessageBuffer],
  ): NioTCPConnection = {

    configureChannel(clientChannel)

    val conn     = NioTCPConnection(clientChannel)
    val callback = incoming.messageHandler(conn)
    clientChannel.register(
      selector,
      SelectionKey.OP_READ,
      ReceiveAttachment(null, incoming, ProtocolState.Plain, conn, callback)
    )
    selector.wakeup()

    conn
  }

  def readN(n: Int, clientChannel: SocketChannel): ByteBuffer = {
    val buffer    = ByteBuffer.allocate(n)
    var bytesRead = 0
    while bytesRead < n do {
      val result = clientChannel.read(buffer)
      if result == -1 then {
        throw NoMoreDataException("remaining channel is empty")
      }
      bytesRead += result
    }
    buffer.flip()
    buffer
  }

  def connect(
      bindsocket: () => SocketChannel,
  ): LatentConnection[MessageBuffer] =
    new LatentConnection {
      override def prepare(incoming: Receive[MessageBuffer]): Async[Any, Connection[MessageBuffer]] =
        Async.fromCallback {
          try
              Async.handler.succeed {
                handleConnection(bindsocket(), incoming)
              }
          catch case NonFatal(exception) => Async.handler.fail(exception)
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

  def listen(
      bindsocket: () => ServerSocketChannel,
  ): LatentConnection[MessageBuffer] =
    new LatentConnection {
      override def prepare(incoming: Receive[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] =
        Async.fromCallback { abort ?=>
          try {
            val serverChannel: ServerSocketChannel = bindsocket()

            val callback = Async.handler[Connection[MessageBuffer]]
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
    val updated = attachment.copy(buffered = attachment.buffered ++ readAvailable(clientChannel))
    val next    = updated.protocol match {
      case ProtocolState.Inspecting         => handleInspecting(clientChannel, updated)
      case ProtocolState.Plain              => handlePlain(updated)
      case ProtocolState.WebSocketHandshake => handleWebSocketHandshake(clientChannel, updated)
      case ProtocolState.WebSocket          => handleWebSocket(updated, clientChannel, key)
    }
    key.attach(next)
    ()
  }

  private def handleInspecting(clientChannel: SocketChannel, attachment: ReceiveAttachment): ReceiveAttachment = {
    if attachment.buffered.length < sniffLength then attachment
    else if WebsocketProtocol.looksLikeHandshake(attachment.buffered) then
        handleWebSocketHandshake(clientChannel, attachment.copy(protocol = ProtocolState.WebSocketHandshake))
    else
        handlePlain(ensurePlainConnected(clientChannel, attachment.copy(protocol = ProtocolState.Plain)))
  }

  private def handlePlain(attachment: ReceiveAttachment): ReceiveAttachment = {
    val callback = attachment.messageCallback.nn
    var buffer   = attachment.buffered

    while
        if buffer.length < 4 then false
        else {
          val len = ByteBuffer.wrap(buffer, 0, 4).getInt
          if len < 0 then throw IllegalStateException(s"negative frame size: $len")
          if buffer.length < 4 + len then false
          else {
            val encodedBytes = java.util.Arrays.copyOfRange(buffer, 4, 4 + len)
            buffer = java.util.Arrays.copyOfRange(buffer, 4 + len, buffer.length)
            pool.execute { () =>
              reporter.received(len + 4)
              val decompressedBytes = if compression then Compression.decompress(encodedBytes) else encodedBytes
              callback.succeed(ArrayMessageBuffer(decompressedBytes))
            }
            true
          }
        }
    do ()

    attachment.copy(buffered = buffer)
  }

  private def handleWebSocketHandshake(
      clientChannel: SocketChannel,
      attachment: ReceiveAttachment
  ): ReceiveAttachment = {
    WebsocketProtocol.tryParseHandshake(attachment.buffered) match {
      case None                      => attachment.copy(protocol = ProtocolState.WebSocketHandshake)
      case Some((request, consumed)) =>
        writeFully(clientChannel, Array(ByteBuffer.wrap(WebsocketProtocol.handshakeResponse(request))))
        val connected = ensureWebSocketConnected(clientChannel, attachment.copy(protocol = ProtocolState.WebSocket))
        handleWebSocket(
          connected.copy(buffered =
            java.util.Arrays.copyOfRange(attachment.buffered, consumed, attachment.buffered.length)
          ),
          clientChannel,
          null,
        )
    }
  }

  private def handleWebSocket(
      attachment: ReceiveAttachment,
      clientChannel: SocketChannel,
      key: SelectionKey | Null
  ): ReceiveAttachment = {
    val callback       = attachment.messageCallback.nn
    var buffer         = attachment.buffered
    var fragments      = attachment.fragments
    var fragmentOpcode = attachment.fragmentOpcode
    var continue       = true

    while continue do
        WebsocketProtocol.tryParseFrame(buffer) match {
          case None =>
            continue = false
          case Some(parsed) =>
            val fin      = WebsocketProtocol.isFinalFrame(parsed.consumed)
            val consumed = WebsocketProtocol.consumedBytes(parsed.consumed)
            buffer = java.util.Arrays.copyOfRange(buffer, consumed, buffer.length)
            reporter.received(consumed)

            parsed.frame match {
              case BinaryFrame(data) =>
                if fin then pool.execute(() => callback.succeed(ArrayMessageBuffer(data)))
                else {
                  fragments = fragments :+ data
                  fragmentOpcode = Some(0x2)
                }

              case TextFrame(text) =>
                val data = text.getBytes(java.nio.charset.StandardCharsets.UTF_8)
                if fin then pool.execute(() => callback.succeed(ArrayMessageBuffer(data)))
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
                  pool.execute(() => callback.succeed(ArrayMessageBuffer(complete)))
                }

              case PingFrame(data) =>
                writeFully(clientChannel, Array(ByteBuffer.wrap(WebsocketProtocol.encodePongFrame(data))))

              case PongFrame(_) =>
                ()

              case CloseFrame(_, _) =>
                try writeFully(clientChannel, Array(ByteBuffer.wrap(WebsocketProtocol.encodeCloseFrame())))
                catch case _: Throwable => ()
                clientChannel.close()
                if key != null then {
                  key.cancel()
                  ()
                }
                continue = false

              case ReservedFrame(_, _) =>
                ()
            }
        }

    attachment.copy(buffered = buffer, fragments = fragments, fragmentOpcode = fragmentOpcode)
  }

  private def ensurePlainConnected(clientChannel: SocketChannel, attachment: ReceiveAttachment): ReceiveAttachment =
    if attachment.connection != null && attachment.messageCallback != null then attachment
    else {
      val conn     = NioTCPConnection(clientChannel)
      val callback = attachment.incoming.messageHandler(conn)
      if attachment.connectCallback != null then attachment.connectCallback.succeed(conn)
      attachment.copy(connection = conn, messageCallback = callback)
    }

  private def ensureWebSocketConnected(clientChannel: SocketChannel, attachment: ReceiveAttachment): ReceiveAttachment =
    if attachment.connection != null && attachment.messageCallback != null then attachment
    else {
      val conn     = WebSocketConnection(clientChannel)
      val callback = attachment.incoming.messageHandler(conn)
      if attachment.connectCallback != null then attachment.connectCallback.succeed(conn)
      attachment.copy(connection = conn, messageCallback = callback)
    }

  private def readAvailable(clientChannel: SocketChannel): Array[Byte] = {
    val target = ByteArrayOutputStream()
    val buffer = ByteBuffer.allocate(readBufferSize)
    var done   = false

    while !done do {
      buffer.clear()
      clientChannel.read(buffer) match {
        case -1 if target.size() == 0 => throw NoMoreDataException("remaining channel is empty")
        case -1                       => done = true
        case 0                        => done = true
        case n                        =>
          buffer.flip()
          val chunk = new Array[Byte](n)
          buffer.get(chunk)
          target.write(chunk)
      }
    }

    target.toByteArray
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
