package ex2026overlaydemo

import channels.*
import channels.connection.{Abort, ConnectionDescriptor, LatentConnection}
import channels.research.{OverlayDemoNode, OverlayStatusProtocol, SignalingServer}
import channels.broadcast.PlumtreeMessage.Payload

import java.net.InetSocketAddress
import java.util.concurrent.Executors

/** vibecoded as part of the hyparview experiments */
object SignalingServerCli {

  def main(args: Array[String]): Unit = {
    val host = args.headOption.getOrElse("localhost")

    val nio         = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
    val nioAbort    = Abort()
    val nioThread   = Executors.newSingleThreadExecutor()
    val nioResolver = new NioTcpConnectionDetailsResolver(nio)

    def bindWebsocketServer(port: Int): (ConnectionDescriptor.TcpWebSocket, LatentConnection[ConnectionDescriptor]) = {
      val socket  = nio.defaultServerSocketChannel(new InetSocketAddress(host, port))()
      val address = socket.getLocalAddress.asInstanceOf[InetSocketAddress]
      (
        ConnectionDescriptor.TcpWebSocket(address.getHostString, address.getPort),
        nio.listen(() => socket)
      )
    }

    val (signalDetails, signalServer) = bindWebsocketServer(9001)

    val (overlayDetails, overlayServer) = bindWebsocketServer(9002)

    val signaling = SignalingServer(debug = true)
    signaling.addIncomingConnection(signalServer)

    given rdts.base.Lattice[Payload[OverlayStatusProtocol.Status]] = summon
    val overlayNode                                                = new OverlayDemoNode(
      selfDetails = Set(overlayDetails),
      listenEnvelope = Some(overlayServer),
      envelopeResolver = nioResolver,
      printOverlayEventsToStdout = true,
    )
    overlayNode.start(Nil)

    nioThread.execute(() => nio.loopSelection(nioAbort))
    val publishedOverlayDetails = overlayNode.selfConnectionDetails.headOption.getOrElse(overlayDetails)
    println(s"&signal=${signalDetails.asUrl}&bootstrap=${publishedOverlayDetails.asUrl}")
  }
}
