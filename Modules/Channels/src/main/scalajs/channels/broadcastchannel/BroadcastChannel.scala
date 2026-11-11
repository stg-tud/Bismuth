package channels.broadcastchannel

import channels.*
import channels.JsArrayBufferMessageBuffer.convertToArrayBuffer
import channels.connection.{Abort, Connection, Connector, MessageBuffer, Receive}
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
  * Each `hello` (e.g., when the other side reloads) establishes a fresh connection.
  */
object BroadcastChannelPairConnector {
  private val hello = "bismuth-pair-hello"
  private val ready = "bismuth-pair-ready"

  def named(name: String): Connector = new Connector {
    override def connect(incomingHandler: Receive): Async[Abort, Connection] = Async.fromCallback {
      val bc                                  = new BroadcastChannel(name)
      var current: Option[Callback[MessageBuffer]] = None

      def establish(): Unit = {
        val connection = BroadcastChannelConnection(bc)
        current = Some(incomingHandler.connectionEstablished(connection))
        Async.handler.succeed(connection)
      }

      bc.onmessage = (event: dom.MessageEvent) =>
        event.data match
            case data: ArrayBuffer =>
              current.foreach(_.succeed(JsArrayBufferMessageBuffer(data)))
            case `hello` =>
              bc.postMessage(ready)
              establish()
            case `ready` =>
              if current.isEmpty then establish()
            case other =>
              current.foreach(_.fail(BroadcastException(s"unexpected message on pair channel ($name):\n$other", event)))

      bc.onmessageerror = (event: dom.MessageEvent) =>
        current.foreach(_.fail(BroadcastException(s"broadcast error ($name):\n${event.data}", event)))

      bc.postMessage(hello)
    }
  }
}
