package channels
package connection

import channels.connection.WebsocketMultiplexing.Framing

import java.net.{InetSocketAddress, SocketAddress, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{CancelledKeyException, SelectionKey, Selector, SocketChannel}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object NioConnection {

  /** A connection on a socket accepted by a listener, or for a websocket client (see
    * [[MultiplexedConnection.startWebsocketClient]]). Established once the protocol of the remote side is known.
    */
  def accepted(channel: SocketChannel, incoming: Receive, deliverExecutor: ExecutionContext): NioConnection =
    NioConnection(channel, incoming, deliverExecutor)

  /** A connection on a socket we dialed, established right away. Speaks plain length-prefixed messages. */
  def dialed(channel: SocketChannel, incoming: Receive, deliverExecutor: ExecutionContext): NioConnection = {
    val conn = NioConnection(channel, incoming, deliverExecutor)
    conn.establish(Framing.Plain)
    conn
  }
}

/** A [[MultiplexedConnection]] on one non-blocking [[NioTCP]] socket. Is the attachment of the selector key of the
  * socket.
  *
  * Threads: receiving ([[receiveAvailable]]), as well as [[continueWriting]] and [[closeWithFailure]], happen on the
  * selector thread only. Sending may happen from any thread, once the connection is established: the send side is
  * guarded by `outLock`.
  *
  * Sending never waits for the socket: what it does not take right away is queued, and written by the selector thread
  * (via [[continueWriting]]) once the socket is writable again. Waiting instead would stop the selector thread from
  * reading, and two peers writing more than the socket buffers hold to each other at once would wait for each other
  * forever. Each send completes once the socket took all of it (or failed): senders that wait for that before sending
  * more get backpressure from slow peers. The queue itself is unbounded.
  */
final class NioConnection private (
    channel: SocketChannel,
    incoming: Receive,
    deliverExecutor: ExecutionContext,
) extends MultiplexedConnection(incoming, deliverExecutor) {

  require(!channel.isBlocking, "the channel must be non-blocking")

  /** A buffer to write, and what to tell once it is written (an empty buffer after those of a write). */
  final private class Pending(val buffer: ByteBuffer, val written: (Try[Unit] => Unit) | Null)

  // send side, guarded by outLock
  private val outLock: AnyRef           = new {}
  private val outgoing                  = java.util.ArrayDeque[Pending]()
  private var key: SelectionKey | Null  = null
  private var closeWhenFlushed: Boolean = false
  private var reading: Boolean          = true

  // ------------------------------------------------------------------------------------------------
  // Connection
  // ------------------------------------------------------------------------------------------------

  override def info: ConnectionInfo = ConnectionInfo(
    local = Try(describe(channel.getLocalAddress)).getOrElse(None),
    remote = Try(describe(channel.getRemoteAddress)).getOrElse(None),
    details = Map("type" -> (framing match
        case Framing.Plain                               => "niotcp"
        case Framing.WebSocket | Framing.WebSocketClient => "websocket"))
  )

  // ------------------------------------------------------------------------------------------------
  // selector events
  // ------------------------------------------------------------------------------------------------

  /** Registers the socket with `selector` for reading, with this as the attachment of its key. From then on, the
    * selector is also asked to call [[continueWriting]] while data is queued.
    */
  def register(selector: Selector): Unit = {
    val registeredKey = channel.register(selector, SelectionKey.OP_READ, this)
    outLock.synchronized {
      key = registeredKey
      updateInterest()
    }
    selector.wakeup(): Unit
  }

  /** Reads what the socket has, and handles all complete messages. */
  def receiveAvailable(): Unit = receive(channel.read)

  /** The socket is writable again: writes more of the queue, as much as the socket takes without waiting. */
  def continueWriting(): Unit = {
    val done = outLock.synchronized(Try(writeQueued()))
    done.failed.foreach(closeSocket)
    // rethrows a failure, for the selector to drop the key
    done.get.foreach(_(Success(())))
  }

  /** Closes the socket right away, failing queued writes and the message callback with `ex`. */
  def closeWithFailure(ex: Throwable): Unit = {
    closeSocket(ex)
    receiveFailed(ex)
  }

  /** Closes the socket right away, failing queued writes with `ex`. */
  private def closeSocket(ex: Throwable): Unit = {
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

  // ------------------------------------------------------------------------------------------------
  // sending
  // ------------------------------------------------------------------------------------------------

  /** Sends all of `buffers`, which must not be changed afterwards. Does not wait: what the socket does not take now is
    * written by the selector thread later. `written` is called once the socket took all of it, or writing failed.
    */
  override protected def write(buffers: Array[ByteBuffer], written: (Try[Unit] => Unit) | Null): Unit = {
    val done = outLock.synchronized {
      if closeWhenFlushed then throw ConnectionClosedException("connection is closed")
      buffers.foreach(b => outgoing.add(Pending(b, null)): Unit)
      if written != null then outgoing.add(Pending(ByteBuffer.allocate(0), written)): Unit
      Try(writeQueued())
    }
    done match
        case Success(done) => done.foreach(_(Success(())))
        case Failure(ex)   => closeSocket(ex)
  }

  /** Stops reading, and closes the socket once the queue is written. */
  override protected def closeAfterWrites(): Unit = outLock.synchronized {
    reading = false
    closeWhenFlushed = true
    if outgoing.isEmpty then channel.close()
    else updateInterest()
  }

  /** Writes as much of the queue as the socket takes without waiting. Must hold `outLock`. Returns the callbacks of
    * the writes completed, to be called without holding the lock.
    */
  private def writeQueued(): List[Try[Unit] => Unit] = {
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

  /** Interested in readability until the remote side closed, in writability exactly while data is queued.
    * Must hold `outLock`.
    */
  private def updateInterest(): Unit = {
    val k = key
    if k != null && k.isValid then
        try {
          val ops = (if reading then SelectionKey.OP_READ else 0) |
            (if outgoing.isEmpty then 0 else SelectionKey.OP_WRITE)
          if k.interestOps() != ops then {
            k.interestOps(ops): Unit
            // a select in progress does not see the change otherwise
            if (ops & SelectionKey.OP_WRITE) != 0 then k.selector().wakeup(): Unit
          }
        } catch case _: CancelledKeyException => ()
  }

  /** Describes an address of this socket. */
  private def describe(address: SocketAddress): Option[ConnectionDescriptor] =
    address match
        case isa: InetSocketAddress       => Some(ConnectionDescriptor.TcpWebSocket(isa.getHostString, isa.getPort))
        case uda: UnixDomainSocketAddress => Some(ConnectionDescriptor.Unix(uda.getPath.toString))
        case _                            => None
}
