package channels.connection

import channels.connection.MessageBuffer.convertByteBufferToArray
import de.rmgk.delay.{Async, Callback}
import rdts.base.Uid

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

  def convertByteBufferToArray(buffer: ByteBuffer): Array[Byte] = {
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
  // TODO: currently not consistently implemented
  def info: ConnectionInfo                    = ConnectionInfo()
  def authenticatedPeerReplicaId: Option[Uid] = None
  def send(message: MessageBuffer): Async[Any, Unit]
  def close(): Unit
}

/** Provides a specification how to handle messages, given a connection context.
  * Failure calls on the callback generally indicate connection errors on the receiver side.
  */
trait Receive {

  /** The provided connection is not guaranteed to be useable until the first message is received.
    * If you want to initiate sending messages on this connection, use the value returned by the prepare call of the latent connection instead.
    */
  def connectionEstablished(answers: Connection): Callback[MessageBuffer]
}

/** Contains all the information required to try and establish a bidirectional connection.
  * Only misses specification on how to handle messages, the abstract handler is acquired from `incoming`
  * Note, may produce multiple connections (e.g., if this produces a server connection) thus triggering the async multiple times.
  *
  * Implementations should make it safe to establish multiple times, though the semantics of that is unclear.
  */
trait LatentConnection[+A] {

  /** The returned async, when run, should establish connections with the given callback atomically.
    * That is, no messages should be lost during setup.
    * Similarly, the provider of the callback (the result of `incoming`) of this method should make sure that the other end of the callback is ready to receive callbacks before running the async.
    *
    * It is generally not assumed to be safe to run prepare twice (neither running a single async twice, nor running two different returned asyncs).
    * Notably, “server” like implementations may try to bind a specific port, and immediately fail if that is not available.
    *
    * The async may produce multiple connections and will run `receiver` for each of them.
    */
  def prepare(receiver: Receive): Async[Abort, A]
}
