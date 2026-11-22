package channels.connection

import channels.MessagingIO

import java.io.*
import java.nio.ByteBuffer
import scala.util.control.NonFatal
import scala.util.{Success, Try}

/** A [[MultiplexedConnection]] on blocking streams. Writes are synchronous, messages are delivered on the receiving
  * thread.
  */
class JIOStreamConnection(in: InputStream, out: OutputStream, doClose: () => Unit, incoming: Receive)
    extends MultiplexedConnection(incoming, MessagingIO.executeImmediately) {

  override protected def write(buffers: Array[ByteBuffer], written: (Try[Unit] => Unit) | Null): Unit = {
    out.synchronized {
      buffers.foreach { buffer =>
        if buffer.hasArray
        then out.write(buffer.array(), buffer.arrayOffset() + buffer.position(), buffer.remaining())
        else out.write(MessageBuffer.convertByteBufferToArray(buffer.duplicate()))
      }
      out.flush()
    }
    if written != null then written(Success(()))
  }

  override protected def closeAfterWrites(): Unit =
      out.close()
      doClose()

  /** Blocks, delivering received messages until the connection ends. */
  def loopReceive(): Unit =
    try receive(target => in.read(target.array(), target.arrayOffset() + target.position(), target.remaining()))
    catch
        case NonFatal(ex) =>
          try doClose()
          catch case NonFatal(_) => ()
          receiveFailed(ex)

}
