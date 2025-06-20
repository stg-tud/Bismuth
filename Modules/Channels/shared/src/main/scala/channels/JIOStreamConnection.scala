package channels

import de.rmgk.delay.{Async, Callback}

import java.io.*

class SendingClosedException extends IOException

class JioInputStreamAdapter(in: InputStream) {
  val inputStream = new DataInputStream(new BufferedInputStream(in))

  def readNext(): MessageBuffer = {
    val size = inputStream.readInt()

    val bytes = new Array[Byte](size)
    inputStream.readFully(bytes, 0, size)

    ArrayMessageBuffer(bytes)
  }

  def loopReceive(handler: Callback[MessageBuffer]): Unit = {
    try
      while true do
        handler.succeed(readNext())
    catch
      case ioe: IOException =>
        handler.fail(ioe)
  }

}

class JioOutputStreamAdapter(out: OutputStream) {

  val outputStream = new DataOutputStream(new BufferedOutputStream(out))

  def send(data: MessageBuffer): Unit = synchronized {
    val outArray = data.asArray
    outputStream.writeInt(outArray.length)
    outputStream.write(outArray)
    outputStream.flush()
  }
}

class JIOStreamConnection(in: InputStream, out: OutputStream, doClose: () => Unit)
    extends Connection[MessageBuffer] {

  // socket streams

  val inputStream  = JioInputStreamAdapter(in)
  val outputStream = JioOutputStreamAdapter(out)

  // connection interface

  def send(data: MessageBuffer): Async[Any, Unit] = Async {
    outputStream.send(data)
  }

  def close(): Unit = doClose()

  // frame parsing

  def loopHandler(handler: Receive[MessageBuffer]): Unit =
    inputStream.loopReceive(handler.messageHandler(this))

}
