package channels.connection

import de.rmgk.delay.{Async, Callback}

import java.io.*

class JioInputStreamAdapter(in: InputStream) {
  val inputStream = new DataInputStream(new BufferedInputStream(in))

  @volatile var closed = false

  def readNext(): MessageBuffer = {
    val size = inputStream.readInt()
    require(size <= MessageBuffer.maxPayloadSize, "Message too large")
    val bytes = new Array[Byte](size)
    inputStream.readFully(bytes, 0, size)

    ArrayMessageBuffer(bytes)
  }

  def loopReceive(handler: Callback[MessageBuffer]): Unit = {
    try
        while !closed do
            handler.succeed(readNext())
        handler.fail(new ConnectionClosedException("requested close"))
    catch
        case ioe: IOException =>
          handler.fail(ioe)
  }

  def close(): Unit =
    closed = true
    in.close()

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
    extends Connection {

  // socket streams

  val inputStream: JioInputStreamAdapter   = JioInputStreamAdapter(in)
  val outputStream: JioOutputStreamAdapter = JioOutputStreamAdapter(out)

  // connection interface

  def send(data: MessageBuffer): Async[Any, Unit] = Async {
    outputStream.send(data)
  }

  def close(): Unit =
    out.close()
    doClose()

  // frame parsing

  def loopHandler(handler: Receive): Unit =
    inputStream.loopReceive(handler.connectionEstablished(this))

}
