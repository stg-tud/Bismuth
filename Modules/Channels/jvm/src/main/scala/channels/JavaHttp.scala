package channels

import com.sun.net.httpserver.{HttpExchange, HttpHandler}
import de.rmgk.delay.{Async, Callback, Sync, toAsync}

import java.io.InputStream
import java.net.URI
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object JavaHttp {

  class SSEServerConnection(out: JioOutputStreamAdapter) extends Connection[MessageBuffer] {
    override def send(message: MessageBuffer): Async[Any, Unit] = Async { out.send(message) }
    override def close(): Unit                                  = out.outputStream.close()
  }

  class SSEServer(addHandler: HttpHandler => Unit) extends LatentConnection[MessageBuffer] {

    def prepare(receiver: Receive[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] = Async.fromCallback {
      addHandler { (exchange: HttpExchange) =>
        val responseHeaders = exchange.getResponseHeaders
        responseHeaders.add("Content-Type", "text/event-stream")
        responseHeaders.add("Connection", "keep-alive")

        exchange.sendResponseHeaders(200, 0)
        val outstream = exchange.getResponseBody

        // force sending of response headers
        outstream.flush()

        val conn = SSEServerConnection(JioOutputStreamAdapter(outstream))

        val cb = receiver.messageHandler(conn)

        val amb = ArrayMessageBuffer(exchange.getRequestBody.readAllBytes())
        if amb.inner.nonEmpty then cb.succeed(amb)
        Async.handler.succeed(conn)
      }
    }
  }

  class SSEClientConnection(client: HttpClient, uri: URI, receiver: Receive[MessageBuffer], ec: ExecutionContext)
      extends Connection[MessageBuffer] {

    lazy val handler: Callback[MessageBuffer] = receiver.messageHandler(this)

    override def send(message: MessageBuffer): Async[Any, Unit] = Async {
      val sseRequest = HttpRequest.newBuilder()
        .POST(BodyPublishers.ofByteArray(message.asArray))
        .uri(uri)
        .build()

      val res = client.sendAsync(sseRequest, BodyHandlers.ofInputStream()).toAsync.bind

      handleReceive(res.body())
    }

    var currentReceive: JioInputStreamAdapter | Null = null

    def handleReceive(rec: InputStream): Unit = synchronized {

      val adapter = JioInputStreamAdapter(rec)

//      if currentReceive != null then {
//        println(s"closing old receiver")
//        currentReceive.nn.close()
//      }
      currentReceive = adapter

      ec.execute(() =>
        adapter.loopReceive {
          case Success(value) =>
            handler.succeed(value)
          case Failure(ex) =>
            if SSEClientConnection.this.synchronized(currentReceive == adapter)
            then
               // if this fails again, then we give up
               send(ArrayMessageBuffer(Array.emptyByteArray)).run(_ => ())
               ()
            else {
              // accept close because another stream seems to be open
              ()
            }
        }
      )
    }
    override def close(): Unit = ()
  }

  class SSEClient(client: HttpClient, uri: URI, ec: ExecutionContext)
      extends LatentConnection[MessageBuffer] {

    def prepare(receiver: Receive[MessageBuffer]): Async[Abort, Connection[MessageBuffer]] = Async {
      val conn = SSEClientConnection(client, uri, receiver, ec)
      conn.send(ArrayMessageBuffer(Array.emptyByteArray)).bind
      conn
    }
  }

}
