package ex2026overlaydemo

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.Uid
import replication.JsoniterCodecs.given
import replication.research.WebRtcSignalingProtocol
import replication.research.WebRtcSignalingProtocol.Message

import java.net.BindException
import java.util.concurrent.Executors
import scala.collection.mutable
import scala.util.{Failure, Success}

/** vibecoded as part of the hyparview experiments */
object WebRtcSignalingServer {

  given JsonValueCodec[WebRtcSignalingProtocol.Session] = JsonCodecMaker.make
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

    val clientsByUid = mutable.Map.empty[Uid, Connection[Message]]
    val uidByConn    = mutable.Map.empty[Connection[Message], Uid]

    def disconnect(conn: Connection[Message]): Unit = {
      uidByConn.remove(conn).foreach(clientsByUid.remove)
      conn.close()
    }

    server.prepare { conn =>
      var registered: Option[Uid] = None
      {
        case Success(Message.Register(uid)) =>
          registered.foreach(clientsByUid.remove)
          clientsByUid.update(uid, conn)
          uidByConn.update(conn, uid)
          registered = Some(uid)
          println(s"[webrtc-signaling] register ${Uid.unwrap(uid)}")

        case Success(msg @ Message.Offer(from, to, _)) =>
          clientsByUid.get(to).foreach(_.send(msg).run(_ => ()))
          println(s"[webrtc-signaling] offer ${Uid.unwrap(from)} -> ${Uid.unwrap(to)}")

        case Success(msg @ Message.Answer(from, to, _)) =>
          clientsByUid.get(to).foreach(_.send(msg).run(_ => ()))
          println(s"[webrtc-signaling] answer ${Uid.unwrap(from)} -> ${Uid.unwrap(to)}")

        case Failure(_) =>
          disconnect(conn)
      }
    }.runIn(nioAbort) {
      case Success(_)  => ()
      case Failure(ex) => ex.printStackTrace()
    }

    nioThread.execute(() => nio.loopSelection(nioAbort))
    println(s"signal=ws://${listenDetails.host}:${listenDetails.port}")
  }
}
