package channels

import channels.connection.{ConnectionClosedException, ConnectionDescriptor, ConnectionInfo}
import rdts.base.Uid

import java.net.{InetSocketAddress, SocketAddress, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import javax.net.ssl.SSLEngine
import javax.net.ssl.SSLEngineResult.{HandshakeStatus, Status}
import scala.util.Try

/** The byte stream of one [[NioTCP]] socket: either the raw channel, or a TLS session on top of it.
  * [[NioTCP]] parses its protocols (plain length-prefixed messages, websockets) on top of this, and writes through it.
  *
  * `read` is only called from the selector thread. `write` may be called from any thread.
  */
sealed trait NioSocketLayer {
  def channel: SocketChannel

  /** Non-blocking: reads available bytes into `targets`. Returns the number of bytes read, or -1 at the end of stream. */
  def read(targets: Array[ByteBuffer]): Long

  /** Writes all of `buffers` (busy waiting if the socket is not writable). */
  def write(buffers: Array[ByteBuffer]): Unit

  /** Bytes that were already received, but not yet returned by `read`, because the targets were full. */
  def hasBufferedInput: Boolean

  /** Whether the layer can transport data (i.e., the TLS handshake is done). */
  def ready: Boolean

  /** Identity of the remote side, as established by this layer. Becomes [[ConnectionInfo.authenticatedPeer]]. */
  def authenticatedPeer: Option[Uid]

  /** Describes an address of this socket (as a descriptor of the kind of connection this layer provides). */
  protected def describe(address: SocketAddress): Option[ConnectionDescriptor]

  /** Local and remote addresses and the authenticated peer of this socket, plus the given details. */
  def connectionInfo(details: (String, String)*): ConnectionInfo = ConnectionInfo(
    local = Try(describe(channel.getLocalAddress)).getOrElse(None),
    remote = Try(describe(channel.getRemoteAddress)).getOrElse(None),
    authenticatedPeer = authenticatedPeer,
    details = details.toMap
  )

  def close(): Unit
}

object NioSocketLayer {

  def writeFully(channel: SocketChannel, buffers: Array[ByteBuffer]): Unit =
    while buffers.exists(_.hasRemaining()) do {
      channel.write(buffers)
      ()
    }

  class Plain(val channel: SocketChannel) extends NioSocketLayer {
    override def read(targets: Array[ByteBuffer]): Long  = channel.read(targets)
    override def write(buffers: Array[ByteBuffer]): Unit = writeFully(channel, buffers)
    override def hasBufferedInput: Boolean               = false
    override def ready: Boolean                          = true
    override def authenticatedPeer: Option[Uid]          = None
    override protected def describe(address: SocketAddress): Option[ConnectionDescriptor] =
      address match
          case isa: InetSocketAddress       => Some(ConnectionDescriptor.TcpWebSocket(isa.getHostString, isa.getPort))
          case uda: UnixDomainSocketAddress => Some(ConnectionDescriptor.Unix(uda.getPath.toString))
          case _                            => None
    override def close(): Unit = channel.close()
  }

  /** TLS on top of a non-blocking channel.
    *
    * The handshake is started on creation and driven by `read`. The authenticated peer is determined by `tls` once the
    * handshake completes. SSLEngine allows wrap and unwrap to run concurrently; wraps (including handshake messages
    * produced while reading) are serialized by `writeLock`.
    */
  class Tls(val channel: SocketChannel, engine: SSLEngine, tls: NioTls) extends NioSocketLayer {
    private val writeLock: AnyRef = new {}
    // receive side, selector thread only. Both buffers are in write mode between calls.
    private var netIn: ByteBuffer = ByteBuffer.allocate(engine.getSession.getPacketBufferSize)
    private var appIn: ByteBuffer = ByteBuffer.allocate(engine.getSession.getApplicationBufferSize)
    private var endOfStream       = false
    // send side, guarded by writeLock
    private var netOut: ByteBuffer = ByteBuffer.allocate(engine.getSession.getPacketBufferSize)

    @volatile private var peer: Option[Uid] = None

    engine.beginHandshake()
    // the client starts by sending its hello
    writeLock.synchronized(handshakeWraps())

    override def ready: Boolean                 = peer.isDefined
    override def authenticatedPeer: Option[Uid] = peer
    override def hasBufferedInput: Boolean      = appIn.position() > 0

    override protected def describe(address: SocketAddress): Option[ConnectionDescriptor] =
      address match
          case isa: InetSocketAddress => Some(ConnectionDescriptor.TcpTls(isa.getHostString, isa.getPort))
          case _                      => None

    override def read(targets: Array[ByteBuffer]): Long = {
      if !endOfStream && channel.read(netIn) == -1 then endOfStream = true
      if !endOfStream then
          if unwrapAvailable() then endOfStream = true
      if peer.isEmpty && engine.getHandshakeStatus == HandshakeStatus.NOT_HANDSHAKING && !endOfStream then
          peer = Some(tls.authenticatedPeer(engine.getSession))

      val copied = drainInto(targets)
      if copied == 0 && endOfStream then -1 else copied
    }

    override def write(buffers: Array[ByteBuffer]): Unit = writeLock.synchronized(wrap(buffers))

    override def close(): Unit = {
      try
          writeLock.synchronized {
            engine.closeOutbound()
            wrap(Array.empty)
          }
      catch case _: Exception => ()
      channel.close()
    }

    private def drainInto(targets: Array[ByteBuffer]): Long = {
      appIn.flip()
      var copied = 0L
      targets.foreach { target =>
        val n = math.min(target.remaining(), appIn.remaining())
        if n > 0 then {
          val slice = appIn.slice(appIn.position(), n)
          target.put(slice)
          appIn.position(appIn.position() + n)
          copied += n
        }
      }
      appIn.compact()
      copied
    }

    private def runDelegatedTasks(): Unit = {
      var task = engine.getDelegatedTask
      while task != null do
          task.run()
          task = engine.getDelegatedTask
    }

    /** Must hold `writeLock`. */
    private def handshakeWraps(): Unit = {
      var continue = true
      while continue do
          engine.getHandshakeStatus match
              case HandshakeStatus.NEED_WRAP => wrap(Array.empty)
              case HandshakeStatus.NEED_TASK => runDelegatedTasks()
              case _                         => continue = false
    }

    /** Decrypts everything decryptable from `netIn` into `appIn`, driving the handshake.
      * Returns true if the peer closed the session.
      */
    private def unwrapAvailable(): Boolean = {
      while true do {
        engine.getHandshakeStatus match
            case HandshakeStatus.NEED_TASK => runDelegatedTasks()
            case HandshakeStatus.NEED_WRAP => writeLock.synchronized(handshakeWraps())
            case _                         =>
              netIn.flip()
              val result =
                try engine.unwrap(netIn, appIn)
                finally netIn.compact(): Unit
              result.getStatus match
                  case Status.OK =>
                    val noProgress = result.bytesConsumed() == 0 && result.bytesProduced() == 0
                    if noProgress && result.getHandshakeStatus != HandshakeStatus.NEED_UNWRAP_AGAIN then return false
                  case Status.BUFFER_UNDERFLOW =>
                    if !netIn.hasRemaining then netIn = grow(netIn, engine.getSession.getPacketBufferSize)
                    return false
                  case Status.BUFFER_OVERFLOW =>
                    appIn = grow(appIn, engine.getSession.getApplicationBufferSize)
                  case Status.CLOSED =>
                    // answer the close_notify, if the engine wants to
                    try writeLock.synchronized(handshakeWraps())
                    catch case _: Exception => ()
                    return true
      }
      false
    }

    /** Encrypts all of `sources` (or a single handshake/close record if empty) and writes it. Must hold `writeLock`. */
    private def wrap(sources: Array[ByteBuffer]): Unit = {
      var wrapOnce = true
      while wrapOnce || sources.exists(_.hasRemaining) do {
        wrapOnce = false
        netOut.clear()
        val result = engine.wrap(sources, netOut)
        result.getStatus match
            case Status.BUFFER_OVERFLOW =>
              netOut = ByteBuffer.allocate(netOut.capacity() * 2)
              wrapOnce = true
            case Status.CLOSED if sources.exists(_.hasRemaining) =>
              throw ConnectionClosedException("TLS session is closed")
            case _ => ()
        if result.getHandshakeStatus == HandshakeStatus.NEED_TASK then runDelegatedTasks()
        netOut.flip()
        writeFully(channel, Array(netOut))
      }
    }

    /** A copy of `buffer` (in write mode) with `additional` more capacity. */
    private def grow(buffer: ByteBuffer, additional: Int): ByteBuffer = {
      val bigger = ByteBuffer.allocate(buffer.capacity() + additional)
      buffer.flip()
      bigger.put(buffer)
      bigger
    }
  }
}
