package channels

import java.net.http.HttpClient
import java.net.URI

class EchoServerTestJavaWebSocketClient extends EchoCommunicationTest[ConnectionDescriptor.TcpWebSocket | ConnectionDescriptor.Unix](
      { ec =>
        val nioTCP = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
        ec.execute(() => nioTCP.loopSelection(Abort()))
        nioTCP.listen()
      },
      _ =>
        case descriptor: ConnectionDescriptor.TcpWebSocket =>
          JavaWebSocketClient.connect(
            HttpClient.newHttpClient(),
            URI.create(s"ws://${descriptor.host}:${descriptor.port}/")
          )
        case descriptor: ConnectionDescriptor.Unix =>
          throw new IllegalStateException("Unix domain sockets not supported")
    )
