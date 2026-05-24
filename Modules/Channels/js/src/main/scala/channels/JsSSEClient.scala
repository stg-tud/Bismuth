package channels

import channels.MesageBufferExtensions.asArrayBuffer
import channels.connection.{Abort, ArrayMessageBuffer, Connection, ConnectionClosedException, LatentConnection, MessageBuffer, Receive}
import de.rmgk.delay.{Async, Callback, toAsync}
import org.scalajs.dom.{AbortController, Headers, HttpMethod, ReadableStreamReader, RequestInit, fetch}

import java.nio.ByteBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}
import scala.util.chaining.scalaUtilChainingOps
import scala.util.{Failure, Success}

object JsSSEClient {

  val connectionIdHeader: String = "X-SSE-Connection-Id"

  class SSEPseudoConnection(
      uri: String,
      receiver: Receive,
      connectionId: String,
      onClose: () => Unit,
  ) extends Connection {

    lazy val resultCallback: Callback[MessageBuffer] = receiver.connectionEstablished(this)

    override def send(message: MessageBuffer): Async[Any, Unit] = Async {

      val requestInit = new RequestInit {}.tap: ri =>
          ri.method = HttpMethod.POST
          ri.body = message.asArrayBuffer
          ri.headers = Headers().tap: hi =>
              hi.set("Accept", "text/event-stream")
              hi.set(connectionIdHeader, connectionId)

      val res = fetch(uri, requestInit).toFuture.toAsync.bind

      val statusCode = res.status
      if statusCode >= 400 then
          throw ConnectionClosedException(s"SSE POST failed with status $statusCode")
    }

    override def close(): Unit = onClose()
  }

  class StreamConsumer(reader: ReadableStreamReader[Uint8Array], cb: Callback[MessageBuffer]) {
    var buffer: Array[Byte] = Array.empty

    var closed = false

    def close(): Unit = {
      closed = true
      reader.cancel()
      ()
    }

    def loop(): Async[Any, Unit] = Async {
      val chunk = reader.read().toFuture.toAsync.bind
      val input = chunk.value
      if !scalajs.js.isUndefined(input) then {
        buffer = buffer.appendedAll(new Int8Array(input.buffer, input.byteOffset, input.length).toArray)

        if buffer.length >= 4 then
            val len = ByteBuffer.wrap(buffer.slice(0, 4)).getInt()
            require(len < MessageBuffer.maxPayloadSize, "Message too large")
            if buffer.length >= len + 4 then
                val mb = ArrayMessageBuffer(buffer.slice(4, len + 4))
                buffer = buffer.slice(len + 4, buffer.length)
                cb.succeed(mb)

        loop().bind
      }
    }
  }

  def connect(uri: String): LatentConnection[Connection] = new LatentConnection[Connection] {
    def prepare(receiver: Receive): Async[Abort, Connection] = Async {

      val getAbort = new AbortController()

      // Establish the SSE event stream via GET
      val getInit = new RequestInit {}.tap: ri =>
          ri.method = HttpMethod.GET
          ri.signal = getAbort.signal
          ri.headers = Headers().tap: hi =>
              hi.set("Accept", "text/event-stream")

      val getResponse = fetch(uri, getInit).toFuture.toAsync.bind

      val connectionId = getResponse.headers.get(connectionIdHeader)

      val reader = getResponse.body.getReader()

      val doAbort = () => {
        reader.cancel()
        getAbort.abort()
      }

      val conn = SSEPseudoConnection(
        uri,
        receiver,
        connectionId,
        doAbort
      )
      val cb = receiver.connectionEstablished(conn)

      val streamConsumer = StreamConsumer(reader, cb)

      streamConsumer.loop().run {
        case Success(value) =>
          doAbort()
          cb.fail(ConnectionClosedException("SSE connection closed"))
        case Failure(ex) =>
          doAbort()
          cb.fail(ex)
      }

      conn
    }
  }

}
