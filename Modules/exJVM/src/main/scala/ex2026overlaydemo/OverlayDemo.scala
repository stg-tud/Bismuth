package ex2026overlaydemo

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, readFromString, writeToArray, writeToString}
import replication.JsoniterCodecs.given
import replication.overlay.HyParViewIO
import replication.overlay.HyParViewStateMachine.HyParViewConfig
import replication.research.OverlayNetworkProtocol.DemoState
import replication.research.SignalingServer.Message
import replication.research.{OverlayDemoNode, SignalingClient}

import java.net.BindException
import java.util.Base64
import java.util.concurrent.Executors
import scala.util.Random

/** vibecoded as part of the hyparview experiments */
object OverlayDemo {

  def connectionString(details: ChannelConnectInfo): String =
    Base64.getUrlEncoder.withoutPadding.encodeToString(
      writeToString(details).getBytes(java.nio.charset.StandardCharsets.UTF_8)
    )

  def parseConnectionString(str: String): ChannelConnectInfo =
    readFromString[ChannelConnectInfo](String(
      Base64.getUrlDecoder.decode(str),
      java.nio.charset.StandardCharsets.UTF_8
    ))

  def jsonConnection[A: JsonValueCodec](latent: LatentConnection, name: String): LatentConnection =
    LatentConnection.adapt[MessageBuffer, A](
      mb => readFromArray[A](mb.asArray),
      a => ArrayMessageBuffer(writeToArray(a)),
      name
    )(latent)

  trait OverlayNodeRuntime {
    def node: OverlayDemoNode
    def stop(): Unit
  }

  object TopicNode {
    def tcp(
             host: String = "127.0.0.1",
             preferredPort: Option[Int] = None,
             signalingServer: Option[ChannelConnectInfo] = None,
             signalingTopic: String = "overlay-demo",
             random: Random = Random(0),
             config: HyParViewConfig = HyParViewConfig.fromEstimatedNetworkSize(10),
             printOverlayEventsToStdout: Boolean = true,
             onStateChanged: DemoState => Unit = _ => (),
    ): TopicNode = {
      val nio         = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
      val nioAbort    = Abort()
      val nioThread   = Executors.newSingleThreadExecutor()
      val nioResolver = new NioTcpConnectionDetailsResolver(nio)

      def listen(port: Int): (ChannelConnectInfo.Tcp, LatentConnection) =
        nioResolver.listen(host, port)

      val (listenDetails, listenBinary) =
        preferredPort match
            case Some(port) =>
              try listen(port)
              catch
                  case _: BindException => listen(0)
            case None => listen(0)

      val listenEnvelope   = jsonConnection(listenBinary, "overlay-json")
      val envelopeResolver = new ChannelResolver {
        override def canConnect(details: ChannelConnectInfo): Boolean =
          nioResolver.canConnect(details)

        override def connect(details: ChannelConnectInfo, label: String)
            : Option[LatentConnection] =
          nioResolver.connect(
            details,
            label
          ).map(jsonConnection[HyParViewIO.Envelope[DemoState]](_, "overlay-json"))
      }

      nioThread.execute(() => nio.loopSelection(nioAbort))

      val node = new OverlayDemoNode(
        selfDetails = Set(listenDetails),
        listenEnvelope = Some(listenEnvelope),
        envelopeResolver = envelopeResolver,
        random = random,
        config = config,
        onStateChanged = onStateChanged,
        printOverlayEventsToStdout = printOverlayEventsToStdout,
      )
      val signalingResolver = new ChannelResolver {
        override def canConnect(details: ChannelConnectInfo): Boolean = nioResolver.canConnect(details)
        override def connect(details: ChannelConnectInfo, label: String): Option[LatentConnection] =
          nioResolver.connect(details, label).map(jsonConnection[Message](_, "signaling-json"))
      }
      val signaling = signalingServer.map { server =>
        new SignalingClient(
          server = server,
          resolver = signalingResolver,
          localUid = node.localUid.uid,
          initialAnnouncements = Map(signalingTopic -> node.selfConnectionDetails),
          onTopicInfo = (_, peers) =>
            node.discoverPeers(peers.iterator.collect {
              case (uid, descriptors) if uid != node.localUid.uid && descriptors.nonEmpty =>
                PeerConnectInfo(uid, descriptors)
            }.toList),
          onPeerInfo = (uid, topics) =>
            topics.values.foreach { descriptors =>
              if uid != node.localUid.uid && descriptors.nonEmpty then
                  node.discoverPeers(PeerConnectInfo(uid, descriptors) :: Nil)
            },
        )
      }
      signaling.foreach { s =>
        s.start()
        s.lookupTopic(signalingTopic).run {
          case scala.util.Success(_)   => ()
          case scala.util.Failure(err) => err.printStackTrace()
        }
      }

      new TopicNode(
        node,
        () => {
          signaling.foreach(_.stop())
          nioAbort.abort()
          nio.selector.wakeup()
          nioThread.shutdownNow()
          ()
        }
      )
    }

    def queued(
        registry: LocalConnectionRegistry,
        id: String,
        random: Random = Random(0),
        config: HyParViewConfig = HyParViewConfig.fromEstimatedNetworkSize(10),
        onStateChanged: DemoState => Unit = _ => (),
    ): TopicNode = {
      val details        = ChannelConnectInfo.QueuedLocal(id)
      val listenEnvelope = registry.queuedServer(details).get
      val node           = new OverlayDemoNode(
        selfDetails = Set(details),
        listenEnvelope = Some(listenEnvelope),
        envelopeResolver = registry,
        random = random,
        config = config,
        onStateChanged = onStateChanged,
        printOverlayEventsToStdout = false,
      )
      new TopicNode(node, () => ())
    }
  }

  class TopicNode(val node: OverlayDemoNode, stopTransport: () => Unit) extends OverlayNodeRuntime {
    def stop(): Unit = {
      node.stop()
      stopTransport()
    }
  }

  class NodeApp(val runtime: OverlayNodeRuntime, seeds: List[ChannelConnectInfo] = Nil) {
    val node: OverlayDemoNode = runtime.node
    node.start(seeds)

    def details: ChannelConnectInfo = node.selfConnectionDetails.head

    def handleInputLine(line: String): Boolean = {
      val trimmed = line.trim
      if trimmed == "q" then false
      else {
        true
      }
    }

    def runConsole(lines: Iterator[String] = scala.io.Source.stdin.getLines()): Unit = {
      var continue = true
      while continue && lines.hasNext do
          continue = handleInputLine(lines.next())
    }

    def stop(): Unit = runtime.stop()
  }

  def main(args: Array[String]): Unit = {
    val (preferredPort, signal, topic, seed) = args.toList match
        case Nil                             => (None, None, "overlay-demo", None)
        case seed :: Nil                     => (None, None, "overlay-demo", Some(parseConnectionString(seed)))
        case "--port" :: port :: Nil         => (Some(port.toInt), None, "overlay-demo", None)
        case "--port" :: port :: seed :: Nil =>
          (Some(port.toInt), None, "overlay-demo", Some(parseConnectionString(seed)))
        case "--signal" :: signal :: Nil => (None, Some(parseConnectionString(signal)), "overlay-demo", None)
        case "--signal" :: signal :: "--topic" :: topic :: Nil =>
          (None, Some(parseConnectionString(signal)), topic, None)
        case "--port" :: port :: "--signal" :: signal :: Nil =>
          (Some(port.toInt), Some(parseConnectionString(signal)), "overlay-demo", None)
        case "--port" :: port :: "--signal" :: signal :: seed :: Nil =>
          (Some(port.toInt), Some(parseConnectionString(signal)), "overlay-demo", Some(parseConnectionString(seed)))
        case "--port" :: port :: "--signal" :: signal :: "--topic" :: topic :: Nil =>
          (Some(port.toInt), Some(parseConnectionString(signal)), topic, None)
        case "--port" :: port :: "--signal" :: signal :: "--topic" :: topic :: seed :: Nil =>
          (Some(port.toInt), Some(parseConnectionString(signal)), topic, Some(parseConnectionString(seed)))
        case _ =>
          println("usage: [--port <port>] [--signal <connection-string>] [--topic <topic>] [<seed-connection-string>]")
          return

    val node = new NodeApp(
      TopicNode.tcp(preferredPort = preferredPort, signalingServer = signal, signalingTopic = topic),
      seeds = seed.toList
    )
    println(s"seed=${connectionString(node.details)}")
    node.runConsole()
  }
}
