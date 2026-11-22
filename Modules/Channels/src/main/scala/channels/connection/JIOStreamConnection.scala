package channels.connection

import channels.connection.WebsocketMultiplexing.{Framing, Output, State}
import de.rmgk.delay.{Async, Callback}

import java.io.*
import scala.util.control.NonFatal

/** A connection on blocking streams, running [[WebsocketMultiplexing]]: plain length-prefixed messages, or a
  * websocket, as the remote side chooses.
  */
class JIOStreamConnection(in: InputStream, out: OutputStream, doClose: () => Unit)
    extends Connection {

  /** Set once, before the connection is handed out. */
  @volatile private var framing: Framing = Framing.Plain
  @volatile private var closed: Boolean  = false

  /** Only accessed by the receiving thread, after [[establish]]. */
  private var messageCallback: Callback[MessageBuffer] | Null = null

  // connection interface

  def send(data: MessageBuffer): Async[Any, Unit] = Async {
    val payload = data.convertToArray()
    write(MessageBuffer.convertByteBufferToArray(WebsocketMultiplexing.header(framing, payload.length)), payload)
  }

  def close(): Unit =
      closed = true
      WebsocketMultiplexing.goodbye(framing).foreach { bytes =>
        try write(bytes)
        catch case NonFatal(_) => ()
      }
      out.close()
      doClose()

  private def write(parts: Array[Byte]*): Unit = out.synchronized {
    parts.foreach(out.write)
    out.flush()
  }

  // frame parsing

  /** Publishes this connection to `incoming`, sending with `framing`. Call before [[loopReceive]] if the protocol is
    * known (e.g., we dialed and speak plain). Otherwise [[loopReceive]] does, once the remote side chose.
    */
  def establish(incoming: Receive, framing: Framing): Unit =
    if messageCallback == null then
        this.framing = framing
        messageCallback = incoming.connectionEstablished(this)

  /** Blocks, delivering received messages until the connection ends. */
  def loopReceive(incoming: Receive): Unit = {
    val buffer = ReceiveBuffer()
    var state  = State.Init
    try
        while state != State.Closed do
            val read = buffer.fill(bb => in.read(bb.array(), bb.arrayOffset() + bb.position(), bb.remaining()))
            if read == -1 then throw NoMoreDataException("remaining channel is empty")
            val (next, outputs) = WebsocketMultiplexing.receive(state, buffer)
            state = next
            outputs.foreach {
              case Output.Detected(framing) => establish(incoming, framing)
              case Output.Message(bytes)    => messageCallback.nn.succeed(ByteBufferMessageBuffer(bytes))
              case Output.Reply(bytes)      => write(bytes)
              case Output.Closed            =>
                messageCallback.nn.fail(NoMoreDataException("websocket closed"))
                out.close()
                doClose()
            }
    catch
        case NonFatal(ex) =>
          try doClose()
          catch case NonFatal(_) => ()
          val callback = messageCallback
          if callback != null then
              callback.fail(if closed then ConnectionClosedException("requested close") else ex)
  }

}
