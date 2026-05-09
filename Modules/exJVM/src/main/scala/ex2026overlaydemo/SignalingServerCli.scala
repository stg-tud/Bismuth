package ex2026overlaydemo

import channels.*
import replication.PlumtreeMessage.Payload
import replication.research.{OverlayDemoNode, OverlayStatusProtocol, SignalingServer}

import java.net.BindException
import java.util.concurrent.Executors

/** vibecoded as part of the hyparview experiments */
object SignalingServerCli {

  def main(args: Array[String]): Unit = {
    val host = args.headOption.getOrElse("localhost")

    val nio         = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
    val nioAbort    = Abort()
    val nioThread   = Executors.newSingleThreadExecutor()
    val nioResolver = new NioTcpConnectionDetailsResolver(nio)

    def listen(port: Int): (ConnectionDescriptor.TcpWebSocket, LatentConnection[ConnectionDescriptor]) = {
      val (details, latent) = nioResolver.listen(host, port)
      (details, latent)
    }

    val (signalDetails, signalServer) =  listen(9001)

    val (overlayDetails, overlayServer) = listen(9002)

    val signaling = SignalingServer(debug = true)
    signaling.addIncomingConnection(signalServer)

    given rdts.base.Lattice[Payload[OverlayStatusProtocol.Status]] = summon
    val overlayNode = new OverlayDemoNode(
      selfDetails = Set(overlayDetails),
      listenEnvelope = Some(overlayServer),
      envelopeResolver = nioResolver,
      printOverlayEventsToStdout = true,
    )
    overlayNode.start(Nil)

    nioThread.execute(() => nio.loopSelection(nioAbort))
    println(s"&signal=${signalDetails.asUrl}&bootstrap=${overlayDetails.asUrl}")
  }
}
