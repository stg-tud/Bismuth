package channels.broadcastchannel

import channels.*
import channels.JsArrayBufferMessageBuffer.convertToArrayBuffer
import channels.connection.{Abort, Connection, ConnectionClosedException, Connector, MessageBuffer, Receive}
import de.rmgk.delay
import de.rmgk.delay.{Async, Callback, Sync}
import org.scalajs.dom
import org.scalajs.dom.{BroadcastChannel, MessageEvent}

import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer

class BroadcastException(message: String, val event: MessageEvent) extends Exception(message)

object BroadcastChannelConnector {
  def named(name: String): Connector = new Connector {
    override def connect(incomingHandler: Receive): Async[Abort, Connection] = Async {

      val bc         = new BroadcastChannel(name)
      val connection = BroadcastChannelConnection(bc)

      val handler = incomingHandler.connectionEstablished(connection)

      bc.onmessage = (event: dom.MessageEvent) =>
        event.data match
            case data: ArrayBuffer =>
              handler.succeed(JsArrayBufferMessageBuffer(data))
            case other =>
              handler.fail(
                BroadcastException(
                  s"someone put something weird on the broadcast channel ($name):\n${event.data}",
                  event
                )
              )

      bc.onmessageerror = (event: dom.MessageEvent) =>
        handler.fail(BroadcastException(s"broadcast error ($name):\n${event.data}", event))

      connection
    }

  }
}

class BroadcastChannelConnection(bc: BroadcastChannel) extends Connection {

  def send(message: MessageBuffer): delay.Async[Any, Unit] =
    Sync(bc.postMessage(message.convertToArrayBuffer()))

  override def close(): Unit = ()

}

/** Point-to-point connection between exactly two contexts (e.g., a page and an iframe it embeds) over a
  * [[BroadcastChannel]] with a name only known to the two of them.
  *
  * Messages posted to a broadcast channel before the other side opened it are lost, thus both sides first exchange a
  * handshake, and the connection is only established once the other side is known to listen.
  * Each `hello` (e.g., when the other side reloads, or [[PairConnector.reconnect]]s) establishes a fresh connection.
  * [[PairConnector.disconnect]] closes the connection on both sides (e.g., to simulate going offline).
  */
object BroadcastChannelPairConnector {
  private val hello = "bismuth-pair-hello"
  private val ready = "bismuth-pair-ready"
  private val bye   = "bismuth-pair-bye"

  class PairConnector(name: String) extends Connector {
    private var bc: BroadcastChannel | Null                                         = null
    private var current: Option[(PairConnection, Callback[MessageBuffer])]          = None
    private var establish: () => Unit                                               = () => ()

    private def closeCurrent(): Unit = {
      current.foreach { (connection, handler) =>
        connection.open = false
        handler.fail(ConnectionClosedException(s"pair channel closed ($name)"))
      }
      current = None
    }

    /** Close the connection on both sides. */
    def disconnect(): Unit = if bc != null then {
      bc.nn.postMessage(bye)
      closeCurrent()
    }

    /** Establish a new connection, if the other side is still listening. */
    def reconnect(): Unit = if bc != null && current.isEmpty then bc.nn.postMessage(hello)

    override def connect(incomingHandler: Receive): Async[Abort, Connection] = Async.fromCallback {
      val channel = new BroadcastChannel(name)
      bc = channel

      establish = () => {
        closeCurrent()
        val connection = PairConnection(channel)
        current = Some((connection, incomingHandler.connectionEstablished(connection)))
        Async.handler.succeed(connection)
      }

      channel.onmessage = (event: dom.MessageEvent) =>
        event.data match
            case data: ArrayBuffer =>
              current.foreach(_._2.succeed(JsArrayBufferMessageBuffer(data)))
            case `hello` =>
              channel.postMessage(ready)
              establish()
            case `ready` =>
              if current.isEmpty then establish()
            case `bye` =>
              closeCurrent()
            case other =>
              current.foreach(_._2.fail(BroadcastException(s"unexpected message on pair channel ($name):\n$other", event)))

      channel.onmessageerror = (event: dom.MessageEvent) =>
        current.foreach(_._2.fail(BroadcastException(s"broadcast error ($name):\n${event.data}", event)))

      channel.postMessage(hello)
    }
  }

  /** Stops sending once closed, such that messages of an old connection never reach a newer one. */
  private class PairConnection(bc: BroadcastChannel) extends Connection {
    var open: Boolean = true

    def send(message: MessageBuffer): delay.Async[Any, Unit] =
      if open then Sync(bc.postMessage(message.convertToArrayBuffer()))
      else Sync(throw ConnectionClosedException("pair channel closed"))

    override def close(): Unit = open = false
  }

  def named(name: String): PairConnector = PairConnector(name)
}
