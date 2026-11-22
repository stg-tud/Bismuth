package jettyWsConnector

import channels.EchoCommunicationTest
import channels.connection.{Abort, ByteBufferMessageBuffer, ConcurrencyHelper, ConnectionDescriptor, NioTCP}
import de.rmgk.delay.*
import org.eclipse.jetty.http.pathmap.PathSpec

import java.net.{InetSocketAddress, URI}

class EchoServerTestJetty extends EchoCommunicationTest(
      { (_, _) =>
        val listener   = JettyWsListener.prepareServer(0)
        val echoServer = listener.listen(PathSpec.from("/registry/*"))
        listener.server.start()
        echoServer
      },
      (_, _) => {
        case ConnectionDescriptor.WebSocket(url) => Some(JettyWsConnection.connect(URI.create(url)))
        case _                                   => None
      }
    ) {
  override def supportsDisconnectDetection: Boolean = false
}

/** The websocket client of [[NioTCP]], connecting to a Jetty websocket server. */
class EchoServerTestJettyWithNioTCPClient extends EchoCommunicationTest(
      { (_, _) =>
        val listener   = JettyWsListener.prepareServer(0)
        val echoServer = listener.listen(PathSpec.from("/registry/*"))
        listener.server.start()
        echoServer
      },
      (ec, abort) => {
        val nioTCP = NioTCP(ConcurrencyHelper.makeExecutionContext(false))
        ec.execute(() => nioTCP.loopSelection(abort))
        {
          case ConnectionDescriptor.WebSocket(url) =>
            val uri = URI.create(url)
            Some(nioTCP.connectWebSocket(
              nioTCP.defaultSocketChannel(InetSocketAddress(uri.getHost, uri.getPort)),
              s"${uri.getHost}:${uri.getPort}",
              uri.getRawPath
            ))
          case _ => None
        }
      }
    ) {
  override def supportsDisconnectDetection: Boolean = false
}

object JettyConnectTest {

  def main(args: Array[String]): Unit = {

    val prepared = JettyWsConnection.connect(URI.create("wss://echo.websocket.org/")).connect: conn =>
        println("established connection")
        msg =>
          println(s"received ${msg.map(_.show)}")

    Async[Abort] {
      val outgoing = prepared.bind
      outgoing.send(ByteBufferMessageBuffer("hello world".getBytes)).bind
      println("send successfull")
    }.runIn(Abort()) { res =>
      println("done!")
      println(res)
    }
  }

}
