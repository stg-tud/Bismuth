package channels

import channels.connection.*
import channels.connection.ConnectionDescriptor

import java.net.URI
import java.net.http.HttpClient

class EchoServerTestJavaWebSocketClient
    extends EchoCommunicationTest(
      { (ec, abort) =>
        val nioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
        ec.execute(() => nioTCP.loopSelection(abort))
        nioTCP.listen()
      },
      (_, _) => {
        case ConnectionDescriptor.TcpWebSocket(host, port) =>
          Some(JavaWebSocketClient.connect(HttpClient.newHttpClient(), URI.create(s"ws://$host:$port/")))
        case _ => None
      }
    )
