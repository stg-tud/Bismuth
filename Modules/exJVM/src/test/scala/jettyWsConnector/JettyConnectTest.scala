package jettyWsConnector

import channels.{Abort, ArrayMessageBuffer, EchoCommunicationTest}
import de.rmgk.delay.*
import org.eclipse.jetty.http.pathmap.PathSpec
import org.eclipse.jetty.server.ServerConnector

import java.net.URI

class EchoServerTestJetty extends EchoCommunicationTest(
      { ec =>
        val listener   = JettyWsListener.prepareServer(0)
        val echoServer = listener.listen(PathSpec.from("/registry/*"))
        listener.server.start()
        val port = listener.server.getConnectors.head.asInstanceOf[ServerConnector].getLocalPort
        (port, echoServer)
      },
      _ => port => JettyWsConnection.connect(URI.create(s"ws://localhost:$port/registry/"))
    )

object JettyConnectTest {

  def main(args: Array[String]): Unit = {

    val prepared = JettyWsConnection.connect(URI.create("wss://echo.websocket.org/")).prepare: conn =>
        println("established connection")
        msg =>
          println(s"received ${msg.map(_.show)}")

    val connect = Async[Abort] {
      val outgoing = prepared.bind
      outgoing.send(ArrayMessageBuffer("hello world".getBytes)).bind
      println("send successfull")
    }.runIn(Abort()) { res =>
      println("done!")
      println(res)
    }
  }

}
