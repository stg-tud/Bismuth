package ex2026overlaydemo

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import replication.JsoniterCodecs.given
import replication.research.SignalingServer.Message
import replication.research.SignalingServer

import java.net.BindException
import java.util.concurrent.Executors

/** vibecoded as part of the hyparview experiments */
object SignalingServerCli {

  given JsonValueCodec[ChannelConnectDescriptor] = JsonCodecMaker.make
  given JsonValueCodec[SignalingServer.Session] = JsonCodecMaker.make
  given JsonValueCodec[Message] = JsonCodecMaker.make

  def main(args: Array[String]): Unit = {
    val (host, preferredPort) = args.toList match
      case Nil => ("127.0.0.1", None)
      case "--port" :: port :: Nil => ("127.0.0.1", Some(port.toInt))
      case host :: Nil => (host, None)
      case host :: "--port" :: port :: Nil => (host, Some(port.toInt))
      case _ =>
        println("usage: [host] [--port <port>]")
        return

    val nio         = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
    val nioAbort    = Abort()
    val nioThread   = Executors.newSingleThreadExecutor()
    val nioResolver = new NioTcpConnectionDetailsResolver(nio)

    def listen(port: Int): (ChannelConnectDescriptor.Tcp, LatentConnection[Message]) = {
      val (details, latent) = nioResolver.listen(host, port)
      (details, OverlayDemo.jsonConnection[Message](latent, "webrtc-signaling-json"))
    }

    val (listenDetails, server) =
      preferredPort match
        case Some(port) =>
          try listen(port)
          catch
            case _: BindException => listen(0)
        case None => listen(0)

    val signaling = SignalingServer(debug = true)
    signaling.addIncomingConnection(server)

    nioThread.execute(() => nio.loopSelection(nioAbort))
    println(s"signal=ws://${listenDetails.host}:${listenDetails.port}")
  }
}
