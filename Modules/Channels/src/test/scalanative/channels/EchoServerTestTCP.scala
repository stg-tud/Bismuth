package channels

import channels.connection.ConnectionDescriptor

import java.net.{InetAddress, InetSocketAddress, ServerSocket, SocketException}

class EchoServerTestTCP extends EchoCommunicationTest(
      { (ec, _) =>
        val socket = new ServerSocket

        try socket.setReuseAddress(true)
        catch {
          case _: SocketException =>
          // some implementations may not allow SO_REUSEADDR to be set
        }

        socket.bind(new InetSocketAddress(InetAddress.getByName("localhost"), 0))

        TCP.listen(() => socket, ec)
      },
      (ec, _) => {
        case ConnectionDescriptor.Tcp(host, port) =>
          Some(TCP.connect(TCP.defaultSocket(new InetSocketAddress(host, port)), ec))
        case _ => None
      }
    )
