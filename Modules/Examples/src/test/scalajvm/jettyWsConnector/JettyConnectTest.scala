package jettyWsConnector

import channels.EchoCommunicationTest
import channels.connection.{Abort, ByteBufferMessageBuffer, ConnectionDescriptor}
import de.rmgk.delay.*
import org.eclipse.jetty.http.pathmap.PathSpec

import java.net.URI

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
