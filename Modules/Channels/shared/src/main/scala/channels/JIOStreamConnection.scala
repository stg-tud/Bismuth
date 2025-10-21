package channels

import de.rmgk.delay.{Async, Callback}

import java.io.*

class JioInputStreamAdapter(in: InputStream) {
  val inputStream = new DataInputStream(new BufferedInputStream(in))

  @volatile var closed = false

  def readNext(): MessageBuffer = {
    val size = inputStream.readInt()

    val bytes = new Array[Byte](size)
    inputStream.readFully(bytes, 0, size)

    ArrayMessageBuffer(bytes)
  }

  def loopReceive(handler: Callback[MessageBuffer]): Unit = {
    try
      while !closed do
        handler.succeed(readNext())
    catch
      case ioe: IOException =>
        handler.fail(ioe)
  }

  def close(): Unit =
    closed = true

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

  val inputStream: JioInputStreamAdapter   = JioInputStreamAdapter(in)
  val outputStream: JioOutputStreamAdapter = JioOutputStreamAdapter(out)

  // connection interface

  def send(data: MessageBuffer): Async[Any, Unit] = Async {
    outputStream.send(data)
  }

  def close(): Unit = doClose()

  // frame parsing

  def loopHandler(handler: Receive[MessageBuffer]): Unit =
    inputStream.loopReceive(handler.messageHandler(this))

}
