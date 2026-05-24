package channels

import channels.connection.{Abort, ArrayMessageBuffer, Connection, ConnectionClosedException, JioInputStreamAdapter, JioOutputStreamAdapter, LatentConnection, MessageBuffer, Receive}
import channels.connection.ConnectionDescriptor
import com.sun.net.httpserver.{HttpExchange, HttpHandler}
import de.rmgk.delay.{Async, Callback, Sync, toAsync}

import java.net.URI
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest}
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object JavaHttpSSE {

  val connectionIdHeader: String = "X-SSE-Connection-Id"

  class SSEServerConnection(val out: JioOutputStreamAdapter, onSendFailure: () => Unit) extends Connection {
    override def send(message: MessageBuffer): Async[Any, Unit] = Async {
      try out.send(message)
      catch
          case ex: Exception =>
            onSendFailure()
            throw ex
    }
    override def close(): Unit = out.outputStream.close()
  }

  class SSEServer(
      addHandler: HttpHandler => Unit,
      descriptor: ConnectionDescriptor.Sse = ConnectionDescriptor.Sse("sse://server")
  ) extends LatentConnection[ConnectionDescriptor.Sse] {

    private val connections = ConcurrentHashMap[String, (SSEServerConnection, Callback[MessageBuffer])]()

    def prepare(receiver: Receive): Async[Abort, ConnectionDescriptor.Sse] = Async {
      addHandler { (exchange: HttpExchange) =>
        val method = exchange.getRequestMethod

        if method.equalsIgnoreCase("GET") then
            val connectionId = java.util.UUID.randomUUID().toString

            val responseHeaders = exchange.getResponseHeaders
            responseHeaders.add("Content-Type", "text/event-stream")
            responseHeaders.add("Connection", "keep-alive")
            responseHeaders.add(connectionIdHeader, connectionId)

            exchange.sendResponseHeaders(200, 0)
            val outstream = exchange.getResponseBody
            outstream.flush()

            val conn =
              SSEServerConnection(JioOutputStreamAdapter(outstream), () => { connections.remove(connectionId); () })
            val cb = receiver.connectionEstablished(conn)

            connections.put(connectionId, (conn, cb))
            ()
        else if method.equalsIgnoreCase("POST") then
            val connectionIdHeaderValue = exchange.getRequestHeaders.getFirst(connectionIdHeader)

            val statusCode =
              if connectionIdHeaderValue != null then
                  connections.get(connectionIdHeaderValue) match
                      case null    => 404
                      case (_, cb) =>
                        val data = ArrayMessageBuffer(exchange.getRequestBody.readAllBytes())
                        if data.inner.nonEmpty then
                            cb.succeed(data)
                            200
                        else 200
              else 400

            exchange.sendResponseHeaders(statusCode, -1)
            exchange.close()
        else
            exchange.sendResponseHeaders(405, -1)
            exchange.close()
      }
      descriptor
    }
  }

  class SSEClientConnection(
      client: HttpClient,
      uri: URI,
      receiver: Receive,
      ec: ExecutionContext,
      connectionId: String,
      jioInputStreamAdapter: JioInputStreamAdapter,
  ) extends Connection {

    lazy val handler: Callback[MessageBuffer] = receiver.connectionEstablished(this)

    override def send(message: MessageBuffer): Async[Any, Unit] = Async {
      val sseRequest = HttpRequest.newBuilder()
        .POST(BodyPublishers.ofByteArray(message.asArray))
        .uri(uri)
        .header(connectionIdHeader, connectionId)
        .build()

      val res        = client.sendAsync(sseRequest, BodyHandlers.discarding()).toAsync.bind
      val statusCode = res.statusCode()
      if statusCode >= 400 then
          throw ConnectionClosedException(s"SSE POST failed with status $statusCode")
    }

    override def close(): Unit =
      jioInputStreamAdapter.close()
  }

  class SSEClient(client: HttpClient, uri: URI, ec: ExecutionContext)
      extends LatentConnection[Connection] {

    def prepare(receiver: Receive): Async[Abort, Connection] = Async {

      val getRequest = HttpRequest.newBuilder()
        .GET()
        .uri(uri)
        .header("Accept", "text/event-stream")
        .build()

      val getResponse = client.sendAsync(getRequest, BodyHandlers.ofInputStream()).toAsync.bind

      val connectionId = getResponse.headers().firstValue(connectionIdHeader).orElseThrow()

      val adapter = JioInputStreamAdapter(getResponse.body())
      val conn = SSEClientConnection(client, uri, receiver, ec, connectionId, adapter)

      ec.execute(() =>
        adapter.loopReceive {
          case Success(value) =>
            conn.handler.succeed(value)
          case Failure(ex) =>
            conn.handler.fail(ex)
        }
      )

      conn
    }
  }

}
