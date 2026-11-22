package channels.connection

import channels.connection.WebsocketMultiplexing.{Framing, Output, State}
import de.rmgk.delay.{Async, Callback}

import java.nio.ByteBuffer
import scala.concurrent.ExecutionContext
import scala.util.Try
import scala.util.control.NonFatal

/** A connection running [[WebsocketMultiplexing]] on a byte stream: plain length-prefixed messages, or a websocket,
  * as the remote side chooses. Transports provide writing and closing, and how to read, see [[receive]].
  * Received messages are delivered using `deliverExecutor`.
  *
  * Receiving ([[receive]], [[receiveFailed]]) happens on one thread at a time. Sending may happen from any thread,
  * once the connection is established.
  */
abstract class MultiplexedConnection(incoming: Receive, deliverExecutor: ExecutionContext) extends Connection {

  /** Set once, before the connection is handed out. */
  @volatile private var currentFraming: Framing = Framing.Plain

  /** We closed: receiving fails because of that. */
  @volatile private var closedLocally: Boolean = false

  // receive side, only accessed by the receiving thread
  private var messageCallback: Callback[MessageBuffer] | Null = null
  private var state: State                                    = State.Init
  private val buffer: ReceiveBuffer                           = ReceiveBuffer()

  /** How messages are framed when sending. */
  final protected def framing: Framing = currentFraming

  /** Writes all of `buffers`, which must not be changed afterwards. Calls `written` once all of it is written, or
    * writing failed, unless this throws.
    */
  protected def write(buffers: Array[ByteBuffer], written: (Try[Unit] => Unit) | Null): Unit

  /** Closes the transport, once what was written before is sent. Nothing more is received. */
  protected def closeAfterWrites(): Unit

  // ------------------------------------------------------------------------------------------------
  // Connection
  // ------------------------------------------------------------------------------------------------

  /** Completes once the transport took the message: senders waiting for that before sending more get backpressure. */
  final override def send(message: MessageBuffer): Async[Any, Unit] = {
    val payload = message.asByteBuffer
    val header  = WebsocketMultiplexing.header(framing, payload.remaining())
    Async.fromCallback {
      val handler = Async.handler[Unit]
      try write(Array(header, payload), handler.complete)
      catch case NonFatal(ex) => handler.fail(ex)
    }
  }

  /** Closes the transport, once what was sent before is written. */
  final override def close(): Unit = {
    closedLocally = true
    WebsocketMultiplexing.goodbye(framing).foreach { bytes =>
      try write(Array(ByteBuffer.wrap(bytes)), null)
      catch case NonFatal(_) => ()
    }
    closeAfterWrites()
  }

  // ------------------------------------------------------------------------------------------------
  // receiving
  // ------------------------------------------------------------------------------------------------

  /** Publishes this connection to `incoming`, sending with `framing`. Call before receiving if the protocol is known
    * (e.g., we dialed and speak plain). Otherwise, this is done once the remote side chose.
    */
  final def establish(framing: Framing): Unit =
    if messageCallback == null then
        currentFraming = framing
        messageCallback = incoming.connectionEstablished(this)

  /** Reads using `read` and handles all complete messages, until `read` returns 0 (nothing more available without
    * waiting), or the remote side closed. `read` gets free space to read into, and returns the number of bytes read
    * (-1 at the end of stream), like NIO channels. Throws [[NoMoreDataException]] at the end of the stream.
    *
    * With a blocking `read`, this receives until the connection ends.
    */
  final protected def receive(read: ByteBuffer => Int): Unit = {
    var count = 0
    while
        count = buffer.fill(read)
        handleBuffered()
        if state == State.Closed then false
        else if count == -1 then throw NoMoreDataException("remaining channel is empty")
        else count > 0
    do ()
  }

  /** Handles all complete messages in the buffer. */
  private def handleBuffered(): Unit = {
    val (next, outputs) = WebsocketMultiplexing.receive(state, buffer)
    state = next
    outputs.foreach {
      case Output.Detected(framing) => establish(framing)
      case Output.Message(bytes)    =>
        val callback = messageCallback.nn
        deliverExecutor.execute(() => callback.succeed(ByteBufferMessageBuffer(bytes)))
      case Output.Reply(bytes) => write(Array(ByteBuffer.wrap(bytes)), null)
      case Output.Closed       =>
        messageCallback.nn.fail(NoMoreDataException("websocket closed"))
        closeAfterWrites()
    }
  }

  /** Receiving failed with `ex`: fails the message callback (with a [[ConnectionClosedException]] if we closed). */
  final protected def receiveFailed(ex: Throwable): Unit = {
    val callback = messageCallback
    if callback != null then callback.fail(if closedLocally then ConnectionClosedException("requested close") else ex)
  }
}
