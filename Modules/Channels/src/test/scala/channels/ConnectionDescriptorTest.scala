package channels

import channels.connection.ConnectionDescriptor

class ConnectionDescriptorTest extends munit.FunSuite {

  test("descriptors with IPv6 hosts are URLs with the address in brackets, and parsed back") {
    List(
      ConnectionDescriptor.Tcp("::1", 4701)                   -> "tcp://[::1]:4701",
      ConnectionDescriptor.TcpTls("fd00:0:0:0:0:0:0:1", 4701) -> "tcp-tls://[fd00:0:0:0:0:0:0:1]:4701",
      ConnectionDescriptor.TcpWebSocket("::", 80)             -> "tcp-ws://[::]:80",
      ConnectionDescriptor.Tcp("10.0.0.1", 4701)              -> "tcp://10.0.0.1:4701",
      ConnectionDescriptor.Udp("example.org", 53)             -> "udp://example.org:53",
    ).foreach { (descriptor, url) =>
      assertEquals(descriptor.asUrl, url)
      assertEquals(ConnectionDescriptor.parse(url), Some(descriptor))
    }
  }
}
