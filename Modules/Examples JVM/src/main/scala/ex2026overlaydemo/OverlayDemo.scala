package ex2026overlaydemo

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.LocalUid
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import replication.JsoniterCodecs.{AWSetStateCodec, ORMapStateCodec}
import replication.PlumtreeDissemination
import replication.overlay.{HyParViewMultiplexed, HyParViewMultiplexedNode, HyParViewUnified}

import java.util.concurrent.Executors
import scala.util.Random

object OverlayDemo {

  given codecString: JsonValueCodec[String] = JsonCodecMaker.make
  given codecConnectionDetails: JsonValueCodec[ConnectionDetails] = JsonCodecMaker.make
  given codecReplicatedSetString: JsonValueCodec[ReplicatedSet[String]] = AWSetStateCodec[String]
  given codecReplicatedSetConnectionDetails: JsonValueCodec[ReplicatedSet[ConnectionDetails]] =
    AWSetStateCodec[ConnectionDetails]
  given codecDirectoryState: JsonValueCodec[ObserveRemoveMap[String, ReplicatedSet[ConnectionDetails]]] =
    ORMapStateCodec[String, ReplicatedSet[ConnectionDetails]]
  given codecOverlayEnvelope: JsonValueCodec[HyParViewMultiplexed.Envelope[ReplicatedSet[String], ConnectionDetails]] =
    HyParViewMultiplexed.envelopeCodec[ReplicatedSet[String], ConnectionDetails]

  enum CoordinationMessage {
    case Register(topic: String, details: ConnectionDetails)
    case Snapshot(state: ObserveRemoveMap[String, ReplicatedSet[ConnectionDetails]])
  }
  given codecCoordinationMessage: JsonValueCodec[CoordinationMessage] = JsonCodecMaker.make

  def jsonConnection[A: JsonValueCodec](latent: LatentConnection[MessageBuffer], name: String): LatentConnection[A] =
    LatentConnection.adapt[MessageBuffer, A](
      mb => readFromArray[A](mb.asArray),
      a => ArrayMessageBuffer(writeToArray(a)),
      name
    )(latent)

  class CoordinationServer(host: String = "127.0.0.1") {
    private val nio       = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
    private val nioAbort  = Abort()
    private val nioThread = Executors.newSingleThreadExecutor()
    private val resolver  = new NioTcpConnectionDetailsResolver(nio)
    private val uid       = LocalUid.gen()
    @volatile private var state: ObserveRemoveMap[String, ReplicatedSet[ConnectionDetails]] = ObserveRemoveMap.empty

    val (details, binaryServer) = resolver.listen(host)
    private val server          = jsonConnection[CoordinationMessage](binaryServer, "coordination-json")

    nioThread.execute(() => nio.loopSelection(nioAbort))

    server.prepare(new Receive[CoordinationMessage] {
      override def messageHandler(conn: Connection[CoordinationMessage]) = {
        case scala.util.Success(CoordinationMessage.Register(topic, peerDetails)) =>
          given LocalUid = uid
          val current = state.get(topic).getOrElse(ReplicatedSet.empty[ConnectionDetails])
          val next    = current.merge(current.add(peerDetails))
          state = state.merge(state.update(topic, next))
          conn.send(CoordinationMessage.Snapshot(state)).run(_ => ())
        case _ => ()
      }
    }).runIn(nioAbort)(_ => ())

    def registerSeed(topic: String, details: ConnectionDetails): Unit = {
      given LocalUid = uid
      val current = state.get(topic).getOrElse(ReplicatedSet.empty[ConnectionDetails])
      val next    = current.merge(current.add(details))
      state = state.merge(state.update(topic, next))
    }

    def stop(): Unit = {
      nioAbort.abort()
      nio.selector.wakeup()
      nioThread.shutdownNow(): Unit
    }
  }

  class TopicNode(topic: String, host: String = "127.0.0.1", random: Random = Random(0)) {
    val localUid: LocalUid = LocalUid.gen()

    private val nio        = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
    private val nioAbort   = Abort()
    private val nioThread  = Executors.newSingleThreadExecutor()
    private val nioResolver = new NioTcpConnectionDetailsResolver(nio)

    @volatile var state: ReplicatedSet[String] = ReplicatedSet.empty[String]

    val (listenDetails, listenBinary) = nioResolver.listen(host)
    private val listenEnvelope = jsonConnection[HyParViewMultiplexed.Envelope[ReplicatedSet[String], ConnectionDetails]](
      listenBinary,
      "overlay-json"
    )

    private val envelopeResolver = new ConnectionDetailsResolver[ConnectionDetails, HyParViewMultiplexed.Envelope[ReplicatedSet[String], ConnectionDetails]] {
      override def connect(details: ConnectionDetails, label: String)
          : Option[LatentConnection[HyParViewMultiplexed.Envelope[ReplicatedSet[String], ConnectionDetails]]] =
        nioResolver.connect(details, label).map { latent =>
          jsonConnection[HyParViewMultiplexed.Envelope[ReplicatedSet[String], ConnectionDetails]](latent, "overlay-json")
        }
    }

    val plumtree: PlumtreeDissemination[ReplicatedSet[String]] = PlumtreeDissemination(
      localUid,
      delta => state = state.merge(delta),
      None
    )

    private val selfRef = HyParViewMultiplexed.PeerRef(localUid.uid, listenDetails: ConnectionDetails)
    private var overlay: Option[HyParViewMultiplexedNode[ReplicatedSet[String], ConnectionDetails]] = None

    nioThread.execute(() => nio.loopSelection(nioAbort))

    def startAsSeed(): Unit = {
      val node = new HyParViewMultiplexedNode(selfRef, plumtree, listenEnvelope, envelopeResolver, None, random)
      overlay = Some(node)
      node.startServer()
    }

    def startAndJoin(contact: ConnectionDetails): Unit = {
      val node = new HyParViewMultiplexedNode(
        selfRef,
        plumtree,
        listenEnvelope,
        envelopeResolver,
        Some(contact),
        random,
        HyParViewUnified.HyParViewConfig.default
      )
      overlay = Some(node)
      node.startServer()
      node.join()
    }

    def publishAdd(value: String): Unit = {
      given LocalUid = localUid
      val delta = state.add(value)
      state = state.merge(delta)
      plumtree.applyDelta(delta)
    }

    def publishRemove(value: String): Unit = {
      val delta = state.remove(value)
      state = state.merge(delta)
      plumtree.applyDelta(delta)
    }

    def activeView: Set[rdts.base.Uid] = overlay.map(_.activeView).getOrElse(Set.empty)

    def stop(): Unit = {
      nioAbort.abort()
      nio.selector.wakeup()
      nioThread.shutdownNow(): Unit
    }
  }

  class ServerApp(topic: String, host: String = "127.0.0.1") {
    val coordination = new CoordinationServer(host)
    val node         = new TopicNode(topic, host)
    node.startAsSeed()
    coordination.registerSeed(topic, node.listenDetails)

    def coordinationDetails: ConnectionDetails.Tcp = coordination.details
    def overlayDetails: ConnectionDetails.Tcp      = node.listenDetails

    def stop(): Unit = {
      node.stop()
      coordination.stop()
    }
  }

  class ClientApp(coordinationDetails: ConnectionDetails.Tcp, topic: String, host: String = "127.0.0.1") {
    val node = new TopicNode(topic, host)

    def start(): Unit = {
      val nio      = new NioTCP(ConcurrencyHelper.makeExecutionContext(false))
      val abort    = Abort()
      val thread   = Executors.newSingleThreadExecutor()
      val resolver = new NioTcpConnectionDetailsResolver(nio)
      thread.execute(() => nio.loopSelection(abort))

      val connLatent = jsonConnection[CoordinationMessage](
        resolver.connect(coordinationDetails, "coord-client").get,
        "coordination-json"
      )

      @volatile var snapshot: Option[ObserveRemoveMap[String, ReplicatedSet[ConnectionDetails]]] = None

      connLatent.prepare(new Receive[CoordinationMessage] {
        override def messageHandler(conn: Connection[CoordinationMessage]) = {
          case scala.util.Success(CoordinationMessage.Snapshot(state)) => snapshot = Some(state)
          case _                                                      => ()
        }
      }).runIn(abort) {
        case scala.util.Success(conn) =>
          conn.send(CoordinationMessage.Register(topic, node.listenDetails)).run(_ => ())
        case _ => ()
      }

      var spins = 0
      while snapshot.isEmpty && spins < 1000 do
        Thread.sleep(5)
        spins += 1

      abort.abort()
      nio.selector.wakeup()
      thread.shutdownNow(): Unit

      val contact = snapshot.flatMap(_.get(topic)).map(_.elements - node.listenDetails).getOrElse(Set.empty).headOption
      contact match
        case Some(peer) => node.startAndJoin(peer)
        case None       => node.startAsSeed()
    }

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
      case "server" :: topic :: Nil =>
        val server = new ServerApp(topic)
        println(s"coordination=${server.coordinationDetails} overlay=${server.overlayDetails}")
      case "client" :: host :: port :: topic :: Nil =>
        val client = new ClientApp(ConnectionDetails.Tcp(host, port.toInt), topic)
        client.start()
        println(s"client overlay=${client.node.listenDetails}")
        client.runConsole()
      case _ =>
        println("usage: server <topic> | client <coord-host> <coord-port> <topic>")
}
