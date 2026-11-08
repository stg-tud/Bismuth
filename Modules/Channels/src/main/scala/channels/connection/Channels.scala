package channels.connection

import channels.connection.MessageBuffer.convertByteBufferToArray
import de.rmgk.delay.{Async, Callback}

import java.nio.ByteBuffer

class NoMoreDataException(msg: String) extends Exception(msg)

trait MessageBuffer {
  def convertToArray(): Array[Byte] = convertByteBufferToArray(asByteBuffer)

  def show: String = convertToArray().mkString("[", ", ", "]")

  /** returns a new ByteBuffer that wraps the underlying memory */
  def asByteBuffer: ByteBuffer
}

object MessageBuffer {
  val maxPayloadSize: Int = 1 << 24

  def convertByteBufferToArray(buffer: ByteBuffer): Array[Byte] =
    if buffer.hasArray then {
      val offset = buffer.arrayOffset() + buffer.position()
      val length = buffer.remaining()
      val arr    = buffer.array()
      if offset == 0 && length == arr.length
      then arr
      else java.util.Arrays.copyOfRange(buffer.array(), offset, offset + length)
    } else {
      val length = buffer.remaining()
      val array  = new Array[Byte](length)
      buffer.get(array)
      array
    }
}

case class ByteBufferMessageBuffer(inner: ByteBuffer) extends MessageBuffer {
  require(inner.remaining() < MessageBuffer.maxPayloadSize, "message too large")
  override def asByteBuffer: ByteBuffer = inner.duplicate()
}

object ByteBufferMessageBuffer {
  def apply(bytes: Array[Byte]): ByteBufferMessageBuffer = ByteBufferMessageBuffer(ByteBuffer.wrap(bytes))
}

class Abort(@volatile var closeRequest: Boolean = false) {
  def abort(): Unit = closeRequest = true
}

/** Connections are bidirectional. Receiving is handled by the incoming handler of the latent connection. */
trait Connection {
  // TODO: info is not implemented by all transports (e.g., WebRTC, JS websockets, local queues)
  def info: ConnectionInfo = ConnectionInfo()
  def send(message: MessageBuffer): Async[Any, Unit]
  def close(): Unit
}

/** Provides a specification how to handle messages, given a connection context.
  * Failure calls on the callback generally indicate connection errors on the receiver side.
  */
trait Receive {

  /** Called once for every new connection, before any message on it is delivered. Returns the callback that
    * receives the messages of `connection`; no received message is lost.
    *
    * `connection` can be sent on from this call on (implementations may delay such sends internally, but do not lose
    * them). Transports that need more information before they can send (e.g., the protocol spoken by the remote side)
    * call this only once they have it.
    */
  def connectionEstablished(connection: Connection): Callback[MessageBuffer]
}

/** Establishes exactly one connection, e.g., by dialing a remote address or by wrapping an existing channel. */
trait Connector {

  /** When run, establishes one connection: `receiver.connectionEstablished(conn)` is called exactly once, and only
    * afterwards the returned async completes with that same `conn`. If the async fails, no connection was established
    * and `connectionEstablished` was not called.
    *
    * Running the async again attempts a new, independent connection (if the implementation supports that).
    */
  def connect(receiver: Receive): Async[Abort, Connection]
}

/** Accepts any number of incoming connections. */
trait Listener {

  /** When run, starts listening and completes with a descriptor under which others can connect to us (it is
    * connectable from then on). `receiver.connectionEstablished` is called once for every accepted connection.
    * Listening stops when the `Abort` is triggered.
    *
    * Not assumed to be safe to run twice (e.g., an implementation may bind a fixed port and fail if it is taken).
    */
  def listen(receiver: Receive): Async[Abort, ConnectionDescriptor]
}
