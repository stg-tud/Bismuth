package channels

import channels.MesageBufferExtensions.asArrayBuffer
import de.rmgk.delay.{Async, Callback, toAsync}
import org.scalajs.dom.{Headers, HttpMethod, ReadableStreamReader, RequestInit, Response, fetch}

import java.nio.ByteBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}
import scala.util.chaining.scalaUtilChainingOps

object JSHttpPseudoChannel {

  class SSEPseudoConnection(uri: String, receiver: Receive[MessageBuffer])
      extends Connection[MessageBuffer] {

    lazy val resultCallback: Callback[MessageBuffer] = receiver.messageHandler(this)

    var currentConsumer: StreamConsumer | Null = null

    override def send(message: MessageBuffer): Async[Any, Unit] = Async {

      val requestInit = new RequestInit {}.tap: ri =>
        ri.method = HttpMethod.POST
        ri.body = message.asArrayBuffer
        ri.headers = Headers().tap: hi =>
          hi.set("Accept", "text/event-stream")

      val res = fetch(uri, requestInit).toFuture.toAsync.bind

      handleResponses(res).bind

    }

    def handleResponses(res: Response): Async[Any, Unit] = Async {

      val reader = res.body.getReader()

      val priorConsumer = currentConsumer
      currentConsumer = StreamConsumer(reader, resultCallback)
      if priorConsumer != null
      then priorConsumer.close()

      currentConsumer.nn.loop().run(using ())(_ => ())
    }

    override def close(): Unit = ()
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
      buffer = buffer.appendedAll(new Int8Array(input.buffer, input.byteOffset, input.length).toArray)

      if buffer.length >= 4 then
        val len = ByteBuffer.wrap(buffer.slice(0, 4)).getInt()
        if buffer.length >= len + 4 then
          val mb = ArrayMessageBuffer(buffer.slice(4, len + 4))
          buffer = buffer.slice(len + 4, buffer.length)
          cb.succeed(mb)

      loop().bind

    }

  }

  def connect(uri: String): LatentConnection[MessageBuffer] = new LatentConnection[MessageBuffer] {
    def prepare(receiver: Receive[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] = Async {

      val conn = new SSEPseudoConnection(uri, receiver)

      conn
    }

  }

}
