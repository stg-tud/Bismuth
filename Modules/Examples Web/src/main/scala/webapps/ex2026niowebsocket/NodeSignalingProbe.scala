package webapps.ex2026niowebsocket

import channels.ChannelConnectDescriptor
import channels.webnativewebsockets.WebSocketConnectionDetailsResolver
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.Uid
import replication.JsoniterCodecs.given
import replication.research.SignalingServer.Message
import replication.research.{SignalingClient, SignalingServer}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.timers.{SetTimeoutHandle, clearTimeout, setTimeout}
import scala.util.{Failure, Success}

object NodeSignalingProbe {

  given JsonValueCodec[ChannelConnectDescriptor] = JsonCodecMaker.make
  given JsonValueCodec[SignalingServer.Session]  = JsonCodecMaker.make
  given JsonValueCodec[Message]                  = JsonCodecMaker.make

  @JSExportTopLevel("NodeSignalingProbe")
  def run(
      signalUrl: String,
      topic: String = "overlay-demo",
      uidString: String = s"node-signaling-probe-${js.Date.now().toLong}",
      timeoutMs: Int = 5000,
  ): js.Promise[String] =
    js.Promise[String] { (resolve, reject) =>
      val resolver = new WebSocketConnectionDetailsResolver[Message]
      val localUid = Uid.predefined(uidString)
      var completed = false
      var timeoutHandle: Option[SetTimeoutHandle] = None

      def finish(result: => String): Unit =
        if !completed then {
          completed = true
          timeoutHandle.foreach(clearTimeout)
          client.stop()
          resolve(result)
          ()
        }

      def fail(message: String): Unit =
        if !completed then {
          completed = true
          timeoutHandle.foreach(clearTimeout)
          client.stop()
          reject(new js.JavaScriptException(message))
          ()
        }

      def log(message: String): Unit =
        println(s"[node-signaling-probe] $message")

      lazy val client: SignalingClient = new SignalingClient(
        server = ChannelConnectDescriptor.WebSocket(signalUrl),
        resolver = resolver,
        localUid = localUid,
        initialAnnouncements = Map(topic -> Set(ChannelConnectDescriptor.WebRtc(uidString))),
        onRegistered = () => {
          log(s"registered uid=$uidString, looking up topic '$topic'")
          client.lookupTopic(topic, 8).run {
            case Success(_)   => ()
            case Failure(err) => fail(s"lookupTopic failed: ${err.getClass.getSimpleName}: ${Option(err.getMessage).getOrElse("")}")
          }
        },
        onTopicInfo = (foundTopic, peers) => {
          log(s"received topic info for '$foundTopic' with peers=${peers.keySet.map(Uid.unwrap).mkString(",")}")
          if foundTopic == topic then
            if peers.contains(localUid) then finish(s"topic-info:$uidString:${peers.size}")
            else fail(s"topic info for '$topic' did not include local uid '$uidString'")
        },
        onPeerInfo = (uid, topics) =>
          log(s"received peer info for ${Uid.unwrap(uid)} with topics=${topics.keySet.mkString(",")}"),
        onOffer = (from, _) =>
          log(s"received offer from ${Uid.unwrap(from)}"),
        onAnswer = (from, _) =>
          log(s"received answer from ${Uid.unwrap(from)}"),
        onDisconnected = () => fail(s"disconnected before probe completed: $signalUrl"),
      )

      timeoutHandle = Some(setTimeout(timeoutMs) {
        fail(s"timed out after ${timeoutMs}ms while probing $signalUrl")
      })

      client.start()
    }
}
