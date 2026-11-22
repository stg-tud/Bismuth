package channels
package connection

import channels.connection.{ConnectionClosedException, ConnectionDescriptor, ConnectionInfo}

import java.net.{InetSocketAddress, SocketAddress, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{CancelledKeyException, SelectionKey, SocketChannel}
import scala.util.{Failure, Success, Try}

/** The byte stream of one [[NioTCP]] socket.
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
final class NioSocketLayer(val channel: SocketChannel) {

  /** A buffer to write, and what to tell once it is written (an empty buffer after those of a write). */
  final private class Pending(val buffer: ByteBuffer, val written: (Try[Unit] => Unit) | Null)

  // send side, guarded by outLock
  private val outLock: AnyRef           = new {}
  private val outgoing                  = java.util.ArrayDeque[Pending]()
  private var key: SelectionKey | Null  = null
  private var closeWhenFlushed: Boolean = false

  /** Sends all of `buffers`, which must not be changed afterwards. Does not wait: what the socket does not take now is
    * written by the selector thread later. `written` is called once the socket took all of it, or writing failed.
    */
  def write(buffers: Array[ByteBuffer], written: (Try[Unit] => Unit) | Null): Unit = {
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

  /** Closes the socket, once what was sent before is written. */
  def close(): Unit = outLock.synchronized {
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

  /** Non-blocking: reads available bytes into `targets`. Returns the number of bytes read, or -1 at the end of stream. */
  def read(targets: Array[ByteBuffer]): Long = channel.read(targets)

  /** [[write]], without caring when it is written. */
  def write(buffers: Array[ByteBuffer]): Unit = write(buffers, null)

  /** Describes an address of this socket. */
  private def describe(address: SocketAddress): Option[ConnectionDescriptor] =
    address match
        case isa: InetSocketAddress       => Some(ConnectionDescriptor.TcpWebSocket(isa.getHostString, isa.getPort))
        case uda: UnixDomainSocketAddress => Some(ConnectionDescriptor.Unix(uda.getPath.toString))
        case _                            => None

  /** Local and remote addresses of this socket, plus the given details. */
  def connectionInfo(details: (String, String)*): ConnectionInfo = ConnectionInfo(
    local = Try(describe(channel.getLocalAddress)).getOrElse(None),
    remote = Try(describe(channel.getRemoteAddress)).getOrElse(None),
    details = details.toMap
  )

}
