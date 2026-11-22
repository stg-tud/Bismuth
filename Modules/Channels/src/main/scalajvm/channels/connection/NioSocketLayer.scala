package channels
package connection

import channels.connection.tls.NioTls
import channels.connection.{ConnectionClosedException, ConnectionDescriptor, ConnectionInfo}
import rdts.base.Uid

import java.net.{InetSocketAddress, SocketAddress, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{CancelledKeyException, SelectionKey, SocketChannel}
import javax.net.ssl.SSLEngine
import javax.net.ssl.SSLEngineResult.{HandshakeStatus, Status}
import scala.util.{Failure, Success, Try}

/** The byte stream of one [[NioTCP]] socket: either the raw channel, or a TLS session on top of it.
  * [[NioTCP]] parses its protocols (plain length-prefixed messages, websockets) on top of this, and writes through it.
  *
  * `read` is only called from the selector thread. `write` may be called from any thread.
  *
  * Writing never waits for the socket: what it does not take right away is queued, and written by the selector thread
  * (via [[flush]]) once the socket is writable again. Waiting instead would stop the selector thread from reading,
  * and two peers writing more than the socket buffers hold to each other at once would wait for each other forever.
  * Each write reports when the socket took all of it (or failed): senders that wait for that before sending more get
  * backpressure from slow peers. The queue itself is unbounded.
  */
sealed trait NioSocketLayer {
  def channel: SocketChannel

  /** A buffer to write, and what to tell once it is written (an empty buffer after those of a write). */
  final private class Pending(val buffer: ByteBuffer, val written: (Try[Unit] => Unit) | Null)

  // send side, guarded by outLock
  private val outLock: AnyRef           = new {}
  private val outgoing                  = java.util.ArrayDeque[Pending]()
  private var key: SelectionKey | Null  = null
  private var closeWhenFlushed: Boolean = false

  /** Queues `buffers` (which must not be changed afterwards) for sending, and writes what the socket takes now.
    * `written` is called once all of them are written, or writing failed.
    */
  protected def enqueue(buffers: Array[ByteBuffer], written: (Try[Unit] => Unit) | Null): Unit = {
    val done = outLock.synchronized {
      if closeWhenFlushed then throw ConnectionClosedException("connection is closed")
      buffers.foreach(b => outgoing.add(Pending(b, null)): Unit)
      if written != null then outgoing.add(Pending(ByteBuffer.allocate(0), written)): Unit
      Try(flushQueue())
    }
    done match
        case Success(done) => done.foreach(_(Success(())))
        case Failure(ex)   => failed(ex)
  }

  /** Writes what is queued as far as the socket takes it without waiting. Called by the selector thread once the socket
    * is writable.
    */
  def flush(): Unit = {
    val done = outLock.synchronized(Try(flushQueue()))
    done.failed.foreach(failed)
    // rethrows a failure, for the selector to drop the key
    done.get.foreach(_(Success(())))
  }

  /** The channel failed: closes it, writes still queued fail with `ex`. */
  def failed(ex: Throwable): Unit = {
    val pending = outLock.synchronized {
      closeWhenFlushed = true
      try channel.close()
      catch case _: java.io.IOException => ()
      val all = List.newBuilder[Try[Unit] => Unit]
      while !outgoing.isEmpty do {
        val p = outgoing.poll().nn
        if p.written != null then all += p.written
      }
      all.result()
    }
    pending.foreach(_(Failure(ex)))
  }

  /** Called once the channel is registered with the selector as `key`: from then on, the selector is asked to call
    * [[flush]] while data is queued.
    */
  def registered(key: SelectionKey): Unit = outLock.synchronized {
    this.key = key
    updateInterest()
  }

  /** Closes the channel once everything queued is written. */
  protected def closeAfterFlush(): Unit = outLock.synchronized {
    closeWhenFlushed = true
    if outgoing.isEmpty then channel.close()
  }

  /** Must hold `outLock`. Returns the callbacks of the writes completed, to be called without holding the lock. */
  private def flushQueue(): List[Try[Unit] => Unit] = {
    val done = List.newBuilder[Try[Unit] => Unit]
    var full = false
    while !full && !outgoing.isEmpty do {
      val head = outgoing.peek().nn
      channel.write(head.buffer): Unit
      if head.buffer.hasRemaining then full = true
      else {
        outgoing.poll(): Unit
        if head.written != null then done += head.written
      }
    }
    if outgoing.isEmpty && closeWhenFlushed then channel.close()
    else updateInterest()
    done.result()
  }

  /** Interested in writability exactly while data is queued. Must hold `outLock`. */
  private def updateInterest(): Unit = {
    val k = key
    if k != null && k.isValid then
        try {
          val ops = if outgoing.isEmpty then SelectionKey.OP_READ else SelectionKey.OP_READ | SelectionKey.OP_WRITE
          if k.interestOps() != ops then {
            k.interestOps(ops): Unit
            // a select in progress does not see the change otherwise
            if (ops & SelectionKey.OP_WRITE) != 0 then k.selector().wakeup(): Unit
          }
        } catch case _: CancelledKeyException => ()
  }

  /** Non-blocking: reads available bytes into `targets`. Returns the number of bytes read, or -1 at the end of stream.
    * May hold back already received data if `targets` are full; callers must call again until it returns 0.
    */
  def read(targets: Array[ByteBuffer]): Long

  /** Sends all of `buffers`, which must not be changed afterwards. Does not wait: what the socket does not take now is
    * written by the selector thread later. `written` is called once the socket took all of it, or writing failed.
    */
  def write(buffers: Array[ByteBuffer], written: (Try[Unit] => Unit) | Null): Unit

  /** [[write]], without caring when it is written. */
  def write(buffers: Array[ByteBuffer]): Unit = write(buffers, null)

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

  /** Closes the socket, once what was sent before is written. */
  def close(): Unit
}

object NioSocketLayer {

  class Plain(val channel: SocketChannel) extends NioSocketLayer {
    override def read(targets: Array[ByteBuffer]): Long                                       = channel.read(targets)
    override def write(buffers: Array[ByteBuffer], written: (Try[Unit] => Unit) | Null): Unit =
      enqueue(buffers, written)
    override def ready: Boolean                                                           = true
    override def authenticatedPeer: Option[Uid]                                           = None
    override protected def describe(address: SocketAddress): Option[ConnectionDescriptor] =
      address match
          case isa: InetSocketAddress       => Some(ConnectionDescriptor.TcpWebSocket(isa.getHostString, isa.getPort))
          case uda: UnixDomainSocketAddress => Some(ConnectionDescriptor.Unix(uda.getPath.toString))
          case _                            => None
    override def close(): Unit = closeAfterFlush()
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

    override def write(buffers: Array[ByteBuffer], written: (Try[Unit] => Unit) | Null): Unit =
      writeLock.synchronized {
        wrap(buffers)
        enqueue(Array.empty, written)
      }

    override def close(): Unit = {
      try
          writeLock.synchronized {
            engine.closeOutbound()
            wrap(Array.empty)
          }
      catch case _: Exception => ()
      closeAfterFlush()
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
      while true do
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
        // netOut is reused: queue a copy
        if netOut.hasRemaining then enqueue(Array(ByteBuffer.allocate(netOut.remaining()).put(netOut).flip()), null)
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
