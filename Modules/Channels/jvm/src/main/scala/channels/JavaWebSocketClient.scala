package channels

import channels.connection.{ArrayMessageBuffer, Connection, ConnectionDescriptor, ConnectionInfo, LatentConnection, MessageBuffer, NoMoreDataException, Receive}
import de.rmgk.delay.{Async, Callback, toAsync}

import java.net.URI
import java.net.http.{HttpClient, WebSocket}
import java.nio.ByteBuffer
import java.util.concurrent.CompletionStage

object JavaWebSocketClient {

  def connect(client: HttpClient, uri: URI): LatentConnection[Connection] =
    new LatentConnection[Connection] {
      override def prepare(incoming: Receive): Async[Any, Connection] =
        Async {
          val listener = new JavaWebSocketListener()
          val socket   = client.newWebSocketBuilder().buildAsync(uri, listener).toAsync.bind
          val conn     = JavaWebSocketConnection(uri, socket)
          listener.install(incoming.connectionEstablished(conn))
          socket.request(1)
          conn
        }
    }
}

final case class JavaWebSocketConnection(uri: URI, socket: WebSocket) extends Connection {
  override val info: ConnectionInfo = ConnectionInfo(
    remote = Some(ConnectionDescriptor.WebSocket(uri.toString)),
    details = Map("type" -> "java-websocket-client")
  )

  override def send(message: MessageBuffer): Async[Any, Unit] =
    socket.sendBinary(ByteBuffer.wrap(message.asArray), true).toAsync.map(_ => ())

  override def close(): Unit = {
    socket.sendClose(WebSocket.NORMAL_CLOSURE, "")
    ()
  }
}

final class JavaWebSocketListener extends WebSocket.Listener {
  @volatile private var callback: Callback[MessageBuffer] | Null = null

  def install(cb: Callback[MessageBuffer]): Unit =
    callback = cb

  override def onOpen(webSocket: WebSocket): Unit =
    ()

  override def onBinary(webSocket: WebSocket, data: ByteBuffer, last: Boolean): CompletionStage[?] = {
    val bytes = new Array[Byte](data.remaining())
    data.get(bytes)
    if last then callback.nn.succeed(ArrayMessageBuffer(bytes))
    else callback.nn.succeed(ArrayMessageBuffer(bytes))
    webSocket.request(1)
    java.util.concurrent.CompletableFuture.completedFuture(null)
  }

  override def onText(webSocket: WebSocket, data: CharSequence, last: Boolean): CompletionStage[?] = {
    val bytes = data.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    if last then callback.nn.succeed(ArrayMessageBuffer(bytes))
    else callback.nn.succeed(ArrayMessageBuffer(bytes))
    webSocket.request(1)
    java.util.concurrent.CompletableFuture.completedFuture(null)
  }

  override def onClose(webSocket: WebSocket, statusCode: Int, reason: String): CompletionStage[?] = {
    if callback != null then callback.nn.fail(NoMoreDataException(s"websocket closed: $statusCode $reason"))
    java.util.concurrent.CompletableFuture.completedFuture(null)
  }

  override def onError(webSocket: WebSocket, error: Throwable): Unit =
    if callback != null then callback.nn.fail(error)
}
