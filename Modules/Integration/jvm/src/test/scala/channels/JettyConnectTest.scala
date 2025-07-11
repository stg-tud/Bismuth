package channels

import channels.Abort
import channels.jettywebsockets.{JettyWsConnection, JettyWsListener}
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

    val prepared = JettyWsConnection.connect(URI.create(s"wss://echo.websocket.org/")).prepare: conn =>
      println(s"established connection")
      msg =>
        println(s"received ${msg.map(_.show)}")

    val connect = Async[Abort] {
      val outgoing = prepared.bind
      outgoing.send(ArrayMessageBuffer("hello world".getBytes)).bind
      println(s"send successfull")
    }.run(using Abort()) { res =>
      println(s"done!")
      println(res)
    }
  }

}
