package ex2026overlaydemo

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, readFromString, writeToArray, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import replication.JsoniterCodecs.{AWSetStateCodec, ORMapStateCodec, given_JsonValueCodec_Uid}
import replication.overlay.{HyParViewMultiplexed, HyParViewUnified}
import replication.research.{OverlayConnectionDirectory, OverlayDemoNode}
import replication.research.OverlayNetworkProtocol.DemoState

import java.util.concurrent.Executors
import scala.util.Random

/** vibecoded. dont trust 😉 */
object OverlayDemo {

  given codecString: JsonValueCodec[String] = JsonCodecMaker.make
  given codecConnectionDetails: JsonValueCodec[ConnectionDetails] = JsonCodecMaker.make
  given codecLinkState: JsonValueCodec[OverlayConnectionDirectory.LinkState] = JsonCodecMaker.make
  given codecConnectedPeer: JsonValueCodec[OverlayConnectionDirectory.ConnectedPeer[ConnectionDetails]] = JsonCodecMaker.make
  given codecNodeInfo: JsonValueCodec[OverlayConnectionDirectory.NodeInfo[ConnectionDetails]] = JsonCodecMaker.make
  given codecReplicatedSetString: JsonValueCodec[ReplicatedSet[String]] = AWSetStateCodec[String]
  given codecReplicatedSetConnectionDetails: JsonValueCodec[ReplicatedSet[ConnectionDetails]] = AWSetStateCodec[ConnectionDetails]
  given codecReplicatedSetConnectedPeer: JsonValueCodec[ReplicatedSet[OverlayConnectionDirectory.ConnectedPeer[ConnectionDetails]]] =
    AWSetStateCodec[OverlayConnectionDirectory.ConnectedPeer[ConnectionDetails]]
  given codecDirectoryState: JsonValueCodec[ObserveRemoveMap[Uid, OverlayConnectionDirectory.NodeInfo[ConnectionDetails]]] =
    ORMapStateCodec[Uid, OverlayConnectionDirectory.NodeInfo[ConnectionDetails]]
  given codecDemoState: JsonValueCodec[DemoState] = JsonCodecMaker.make
  given codecOverlayEnvelope: JsonValueCodec[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]] =
    HyParViewMultiplexed.envelopeCodec[DemoState, ConnectionDetails]

  def connectionString(details: ConnectionDetails): String = writeToString(details)

  def parseConnectionString(str: String): ConnectionDetails = readFromString[ConnectionDetails](str)

  def jsonConnection[A: JsonValueCodec](latent: LatentConnection[MessageBuffer], name: String): LatentConnection[A] =
    LatentConnection.adapt[MessageBuffer, A](
      mb => readFromArray[A](mb.asArray),
      a => ArrayMessageBuffer(writeToArray(a)),
      name
    )(latent)

  object TopicNode {
    def tcp(
        host: String = "127.0.0.1",
        random: Random = Random(0),
        onDirectoryChanged: OverlayConnectionDirectory.Directory[ConnectionDetails] => Unit = _ => (),
    ): TopicNode = {
      val nio         = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
      val nioAbort    = Abort()
      val nioThread   = Executors.newSingleThreadExecutor()
      val nioResolver = new NioTcpConnectionDetailsResolver(nio)

      val (listenDetails, listenBinary) = nioResolver.listen(host)
      val listenEnvelope = jsonConnection[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]](
        listenBinary,
        "overlay-json"
      )

      val envelopeResolver = new ConnectionDetailsResolver[ConnectionDetails, HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]] {
        override def connect(details: ConnectionDetails, label: String)
            : Option[LatentConnection[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]]] =
          nioResolver.connect(details, label).map { latent =>
            jsonConnection[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]](latent, "overlay-json")
          }
      }

      nioThread.execute(() => nio.loopSelection(nioAbort))

      new TopicNode(
        listenDetails = listenDetails,
        listenEnvelope = listenEnvelope,
        envelopeResolver = envelopeResolver,
        stopTransport = () => {
          nioAbort.abort()
          nio.selector.wakeup()
          nioThread.shutdownNow()
          ()
        },
        random = random,
        config = HyParViewUnified.HyParViewConfig.fromEstimatedNetworkSize(10),
        onDirectoryChanged = onDirectoryChanged,
      )
    }
  }

  class TopicNode(
      val listenDetails: ConnectionDetails,
      listenEnvelope: LatentConnection[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]],
      envelopeResolver: ConnectionDetailsResolver[ConnectionDetails, HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]],
      stopTransport: () => Unit,
      random: Random = Random(0),
      config: HyParViewUnified.HyParViewConfig,
      onDirectoryChanged: OverlayConnectionDirectory.Directory[ConnectionDetails] => Unit = _ => (),
  ) {
    val core = new OverlayDemoNode(
      selfDetails = Set(listenDetails),
      membershipDetails = Some(listenDetails),
      listenEnvelope = Some(listenEnvelope),
      envelopeResolver = envelopeResolver,
      random = random,
      config = config,
      onDirectoryChanged = onDirectoryChanged,
    )

    def localUid: LocalUid = core.localUid
    def state: DemoState = core.state
    def start(seeds: List[ConnectionDetails] = Nil): Unit = core.start(seeds)
    def publishAdd(value: String): Unit = core.publishAdd(value)
    def publishRemove(value: String): Unit = core.publishRemove(value)
    def activeView: Set[Uid] = core.activeView
    def passiveView: Set[Uid] = core.passiveView
    def connectionDirectory: OverlayConnectionDirectory.Directory[ConnectionDetails] = core.connectionDirectory

    def stop(): Unit = {
      core.stop()
      stopTransport()
    }
  }

  class NodeApp(host: String = "127.0.0.1", seeds: List[ConnectionDetails] = Nil) {
    val node = TopicNode.tcp(host)
    node.start(seeds)

    def details: ConnectionDetails = node.listenDetails

    def handleInputLine(line: String): Boolean = {
      val trimmed = line.trim
      if trimmed == "q" then false
      else {
        if trimmed.nonEmpty then
          if trimmed.startsWith("-") then node.publishRemove(trimmed.drop(1))
          else node.publishAdd(trimmed)
        true
      }
    }

    def runConsole(lines: Iterator[String] = scala.io.Source.stdin.getLines()): Unit = {
      var continue = true
      while continue && lines.hasNext do
        continue = handleInputLine(lines.next())
    }

    def stop(): Unit = node.stop()
  }

  def main(args: Array[String]): Unit =
    args.toList match
      case Nil =>
        val node = new NodeApp()
        println(s"seed=${connectionString(node.details)}")
        node.runConsole()
      case seed :: Nil =>
        val node = new NodeApp(seeds = List(parseConnectionString(seed)))
        println(s"seed=${connectionString(node.details)}")
        node.runConsole()
      case _ =>
        println("usage: [<seed-connection-string>]")
}
