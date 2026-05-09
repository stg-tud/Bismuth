package jettyWsConnector

import channels.{Abort, ArrayMessageBuffer, Connection, ConnectionDescriptor, LatentConnection, MessageBuffer, Receive as ChannelHandler}
import de.rmgk.delay.{Async, toAsync, Callback as DelayCallback}
import org.eclipse.jetty.http.pathmap.PathSpec
import org.eclipse.jetty.server.handler.{ContextHandler, ContextHandlerCollection}
import org.eclipse.jetty.server.{Server, ServerConnector}
import org.eclipse.jetty.util.Callback as JettyUtilCallback
import org.eclipse.jetty.websocket.api.Session.Listener
import org.eclipse.jetty.websocket.api.{Session, Callback as JettyCallback}
import org.eclipse.jetty.websocket.client.WebSocketClient
import org.eclipse.jetty.websocket.server.*

import java.net.URI
import java.nio.ByteBuffer

def println(str: Any): Unit = System.out.println(s"$str [${Thread.currentThread().getName()}]")

object JettyWsListener {

  def prepareServer(port: Int): JettyWsListener = {
    val server = new Server()

    val connector = new ServerConnector(server)
    connector.setPort(port)
    server.addConnector(connector)

    val jettyWsListener = fromServer(server)
    server.setHandler(jettyWsListener.handlers)
    jettyWsListener
  }

  def fromServer(server: Server) =
    new JettyWsListener(server)

}

class JettyWsListener(val server: Server) {

  val handlers = new ContextHandlerCollection()

  def listen(
      pathSpec: PathSpec,
      context: ContextHandler = new ContextHandler()
  ): LatentConnection[ConnectionDescriptor] =
    new LatentConnection[ConnectionDescriptor] {
      override def prepare(incomingHandler: ChannelHandler): Async[Abort, ConnectionDescriptor] =
        Async {

          val webSocketHandler = WebSocketUpgradeHandler.from(
            server,
            context,
            (wsContainer: ServerWebSocketContainer) => {
              wsContainer.addMapping(
                pathSpec,
                webSocketCreator(incomingHandler)
              )
            }
          )
          context.setHandler(webSocketHandler)
          handlers.addHandler(context)
          context.start()
          ConnectionDescriptor.WebSocket("jetty-ws://listener")
        }
    }

  def webSocketCreator(
      incoming: ChannelHandler
  ): WebSocketCreator =
    new WebSocketCreator {
      override def createWebSocket(
          request: ServerUpgradeRequest,
          upgradeResponse: ServerUpgradeResponse,
          // callback has to be ignored if a handler is returned (great design jetty!)
          callback: JettyUtilCallback
      ): AnyRef =
        new JettyWsHandler(incoming, _ => ())
    }
}

object JettyWsConnection {

  def connect(uri: URI): LatentConnection[Connection] = new LatentConnection[Connection] {
    override def prepare(incomingHandler: ChannelHandler): Async[Abort, Connection] =
      Async.fromCallback[Connection] {

        val client = new WebSocketClient()
        client.start()
        // this returns a future
        client.connect(
          new JettyWsHandler(incomingHandler, Async.handler[Connection]),
          uri
        ).toAsync.run {
          sess =>
        }
      }
  }
}

class JettySessionWrapper(session: Session) extends Connection {

  override def close(): Unit =
    session.close()

  override def send(data: MessageBuffer): Async[Any, Unit] = Async.fromCallback {
    session.sendBinary(
      ByteBuffer.wrap(data.asArray),
      new JettyCallback {
        override def succeed(): Unit          = Async.handler.succeed(())
        override def fail(x: Throwable): Unit = Async.handler.fail(x)
      }
    )
  }
}

class JettyWsHandler(
    incoming: ChannelHandler,
    connectionEstablished: DelayCallback[Connection]
) extends Listener.Abstract {

  @volatile private var internalCallback: DelayCallback[MessageBuffer] = scala.compiletime.uninitialized

  override def onWebSocketOpen(session: Session): Unit = {
    super.onWebSocketOpen(session)
    val sessionWrapper = new JettySessionWrapper(session)
    internalCallback = incoming.connectionEstablished(sessionWrapper)
    connectionEstablished.succeed(sessionWrapper)
    session.demand()
  }

  override def onWebSocketBinary(buffer: ByteBuffer, callback: JettyCallback): Unit = {

    val data = new Array[Byte](buffer.remaining())
    buffer.get(data)

    internalCallback.succeed(ArrayMessageBuffer(data))

    callback.succeed()
    getSession.demand()
  }

  override def onWebSocketText(message: String): Unit =
    getSession.demand()

  override def onWebSocketClose(statusCode: Int, reason: String): Unit = {}

  override def onWebSocketError(cause: Throwable): Unit =
    internalCallback.fail(cause)

  override def onWebSocketPing(payload: ByteBuffer): Unit =
    getSession.sendPong(payload, JettyCallback.from(() => getSession.demand(), _ => ()))
}
