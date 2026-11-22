package channels.connection.tls

import channels.connection.{ByteBufferMessageBuffer, Connection, ConnectionClosedException, ConnectionInfo, MessageBuffer, NoMoreDataException, Receive}
import de.rmgk.delay.{Async, Callback}

import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicReference
import javax.net.ssl.SSLEngine
import javax.net.ssl.SSLEngineResult.{HandshakeStatus, Status}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** One TLS session on top of an inner connection (see [[Tls.connect]] and [[Tls.listen]]).
  *
  * The [[SSLEngine]] does no IO, it only turns plaintext into TLS records (`wrap`) and back (`unwrap`), and says what
  * the handshake needs next. Records produced are sent as messages of the inner connection, received messages are
  * unwrapped. The inner connection must be reliable and ordered, but message boundaries do not matter to TLS: a
  * record may span messages. TLS carries a byte stream, so each message is sent with its length in front.
  *
  * Once the handshake is done, the [[Connection]] on top is reported to `receive` and to `established`.
  * If the session fails before, only `established` is told.
  *
  * The engine allows one wrap and one unwrap to run at once: wraps (including handshake messages produced while
  * receiving) are serialized by `sendLock`, received messages by `receiveLock`.
  */
final private[tls] class TlsSession(
    engine: SSLEngine,
    tls: Tls,
    receive: Receive,
    established: Try[Connection] => Unit
) {

  @volatile private var inner: Connection | Null = null

  // receive side, guarded by receiveLock. Both buffers are in write mode between calls.
  private val receiveLock: AnyRef                      = new {}
  private var netIn: ByteBuffer                        = ByteBuffer.allocate(engine.getSession.getPacketBufferSize)
  private var appIn: ByteBuffer                        = ByteBuffer.allocate(engine.getSession.getApplicationBufferSize)
  private var outer: Connection | Null                 = null
  private var callback: Callback[MessageBuffer] | Null = null
  private var finished: Boolean                        = false

  // send side, guarded by sendLock
  private val sendLock: AnyRef   = new {}
  private var netOut: ByteBuffer = ByteBuffer.allocate(engine.getSession.getPacketBufferSize)

  /** Wrapped records are sent in inner messages of about this size. */
  private val batchSize: Int = 1 << 20

  /** Starts the handshake on `inner`, returns the callback for the messages received on it. */
  def start(inner: Connection): Callback[MessageBuffer] = {
    this.inner = inner
    try {
      engine.beginHandshake()
      // the client starts by sending its hello
      sendLock.synchronized(handshakeWraps())
    } catch case NonFatal(ex) => fail(ex)
    {
      case Success(message) =>
        receiveLock.synchronized {
          try received(message)
          catch case NonFatal(ex) => fail(ex)
        }
      case Failure(ex) => fail(ex)
    }
  }

  /** Must hold `receiveLock`. */
  private def received(message: MessageBuffer): Unit = {
    if finished then return
    val bytes = message.asByteBuffer
    if netIn.remaining() < bytes.remaining() then netIn = grow(netIn, bytes.remaining())
    netIn.put(bytes)
    val peerClosed = unwrapAvailable()
    if outer == null && !peerClosed && engine.getHandshakeStatus == HandshakeStatus.NOT_HANDSHAKING then establish()
    deliverMessages()
    if peerClosed then fail(NoMoreDataException("TLS session closed by peer"))
  }

  /** Must hold `receiveLock`. */
  private def establish(): Unit = {
    val innerInfo = inner.nn.info
    val conn      = new Connection {
      override val info: ConnectionInfo = ConnectionInfo(
        local = innerInfo.local.map(Tls.describe),
        remote = innerInfo.remote.map(Tls.describe),
        authenticatedPeer = Some(tls.authenticatedPeer(engine.getSession)),
        details = innerInfo.details
      )
      override def send(message: MessageBuffer): Async[Any, Unit] = TlsSession.this.send(message)
      override def close(): Unit                                  = TlsSession.this.close()
    }
    outer = conn
    callback = receive.connectionEstablished(conn)
    established(Success(conn))
  }

  /** Delivers the complete messages in `appIn`, each with its length in front. Must hold `receiveLock`. */
  private def deliverMessages(): Unit = {
    val cb = callback
    if cb == null then return
    appIn.flip()
    var continue = true
    while continue do
        if appIn.remaining() >= 4 && appIn.remaining() >= 4 + appIn.getInt(appIn.position()) then {
          val bytes = new Array[Byte](appIn.getInt())
          appIn.get(bytes)
          cb.succeed(ByteBufferMessageBuffer(bytes))
        } else continue = false
    appIn.compact(): Unit
  }

  /** Decrypts everything decryptable from `netIn` into `appIn`, driving the handshake.
    * Returns true if the peer closed the session. Must hold `receiveLock`.
    */
  private def unwrapAvailable(): Boolean = {
    while true do
        engine.getHandshakeStatus match
            case HandshakeStatus.NEED_TASK => runDelegatedTasks()
            case HandshakeStatus.NEED_WRAP => sendLock.synchronized(handshakeWraps())
            case _                         =>
              netIn.flip()
              val result =
                try engine.unwrap(netIn, appIn)
                finally netIn.compact(): Unit
              result.getStatus match
                  case Status.OK =>
                    val noProgress = result.bytesConsumed() == 0 && result.bytesProduced() == 0
                    if noProgress && result.getHandshakeStatus != HandshakeStatus.NEED_UNWRAP_AGAIN then return false
                  case Status.BUFFER_UNDERFLOW => return false
                  case Status.BUFFER_OVERFLOW  =>
                    appIn = grow(appIn, engine.getSession.getApplicationBufferSize)
                  case Status.CLOSED =>
                    // answer the close_notify, if the engine wants to
                    try sendLock.synchronized(handshakeWraps())
                    catch case NonFatal(_) => ()
                    return true
    false
  }

  /** Fails the session once: before the handshake is done the connector, afterwards the receiver. */
  private def fail(ex: Throwable): Unit = {
    val report = receiveLock.synchronized {
      if finished then None
      else {
        finished = true
        Some(callback)
      }
    }
    report.foreach { cb =>
      if cb == null then established(Failure(ex)) else cb.fail(ex)
      val in = inner
      if in != null then in.close()
    }
  }

  def send(message: MessageBuffer): Async[Any, Unit] = Async.fromCallback {
    val handler = Async.handler[Unit]
    try
        sendLock.synchronized {
          val payload = message.asByteBuffer
          val length  = ByteBuffer.allocate(4).putInt(payload.remaining()).flip()
          wrapAndSend(Array(length, payload), handler.complete)
        }
    catch case NonFatal(ex) => handler.fail(ex)
  }

  def close(): Unit = {
    try
        sendLock.synchronized {
          engine.closeOutbound()
          wrapAndSend(Array.empty, null)
        }
    catch case NonFatal(_) => ()
    val in = inner
    if in != null then in.close()
  }

  /** Must hold `sendLock`. */
  private def handshakeWraps(): Unit = {
    var continue = true
    while continue do
        engine.getHandshakeStatus match
            case HandshakeStatus.NEED_WRAP => wrapAndSend(Array.empty, null)
            case HandshakeStatus.NEED_TASK => runDelegatedTasks()
            case _                         => continue = false
  }

  /** Encrypts all of `sources` (or a single handshake/close record if empty) and sends the records on the inner
    * connection. `written` is told once the last of them is sent. Must hold `sendLock`.
    */
  private def wrapAndSend(sources: Array[ByteBuffer], written: (Try[Unit] => Unit) | Null): Unit = {
    val failure                                           = AtomicReference[Throwable | Null](null)
    def sendBatch(batch: ByteBuffer, last: Boolean): Unit =
      inner.nn.send(ByteBufferMessageBuffer(batch.flip())).run {
        case Failure(ex) if !last              => failure.compareAndSet(null, ex): Unit
        case result if last && written != null =>
          val earlier = failure.get()
          written(if earlier != null then Failure(earlier) else result)
        case _ => ()
      }

    // a full batch is only sent once the next one starts: the last one sent tells `written`
    var full: ByteBuffer | Null = null
    var batch                   = ByteBuffer.allocate(0)
    var wrapOnce                = true
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
      if batch.remaining() < netOut.remaining() then batch = grow(batch, math.max(netOut.remaining(), batchSize))
      batch.put(netOut)
      if batch.position() >= batchSize then {
        if full != null then sendBatch(full, last = false)
        full = batch
        batch = ByteBuffer.allocate(0)
      }
    }
    if batch.position() > 0 then {
      if full != null then sendBatch(full, last = false)
      full = batch
    }
    if full != null then sendBatch(full, last = true)
    else if written != null then written(Success(()))
  }

  private def runDelegatedTasks(): Unit = {
    var task = engine.getDelegatedTask
    while task != null do
        task.run()
        task = engine.getDelegatedTask
  }

  /** A copy of `buffer` (in write mode) with `additional` more capacity. */
  private def grow(buffer: ByteBuffer, additional: Int): ByteBuffer = {
    val bigger = ByteBuffer.allocate(buffer.capacity() + additional)
    buffer.flip()
    bigger.put(buffer)
    bigger
  }
}
