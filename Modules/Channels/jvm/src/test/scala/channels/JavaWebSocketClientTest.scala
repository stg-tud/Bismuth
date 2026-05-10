package channels

import java.net.http.HttpClient
import java.net.URI
import java.nio.channels.Selector

class EchoServerTestJavaWebSocketClient extends EchoCommunicationTest[ConnectionDescriptor.TcpWebSocket | ConnectionDescriptor.Unix](
      { (ec, abort) =>
        val nioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
        ec.execute(() => nioTCP.loopSelection(abort))
        nioTCP.listen()
      },
      (_, _) =>
        case descriptor: ConnectionDescriptor.TcpWebSocket =>
          JavaWebSocketClient.connect(
            HttpClient.newHttpClient(),
            URI.create(s"ws://${descriptor.host}:${descriptor.port}/")
          )
        case descriptor: ConnectionDescriptor.Unix =>
          throw new IllegalStateException("Unix domain sockets not supported")
    )
