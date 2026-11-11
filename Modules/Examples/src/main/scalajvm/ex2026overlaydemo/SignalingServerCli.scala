package ex2026overlaydemo

import channels.connection.*
import channels.*
import channels.broadcast.PlumtreeMessage.Payload
import channels.connection.{Abort, ConnectionDescriptor, Listener}
import rdts.base.Lattice

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

    def bindWebsocketServer(port: Int): (ConnectionDescriptor.TcpWebSocket, Listener) = {
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

    given rdts.base.Lattice[Payload[OverlayStatusProtocol.Status]] = Lattice.derived
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
