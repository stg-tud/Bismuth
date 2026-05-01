package webapps.ex2026niowebsocket

import channels.*
import channels.webnativewebsockets.WebSocketConnectionDetailsResolver
import channels.webrtc.{SessionDescription, WebRTCConnection, WebRTCConnector}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, readFromString, writeToArray, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.Async
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, HTMLCanvasElement, document, window}
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import replication.JsoniterCodecs.{AWSetStateCodec, ORMapStateCodec, given}
import replication.overlay.HyParViewMultiplexed
import replication.research.{OverlayConnectionDirectory, OverlayDemoNode, WebRtcSignalingProtocol}
import replication.research.OverlayConnectionDirectory.LinkState
import replication.research.OverlayNetworkProtocol.DemoState
import replication.research.WebRtcSignalingProtocol.Message
import scalatags.JsDom.all.*

import java.util.Base64
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.{Failure, Success}

/** vibecoded as part of the hyparview experiments */
object OverlayNetworkGraph {

  given codecString: JsonValueCodec[String] = JsonCodecMaker.make
  given codecConnectionDetails: JsonValueCodec[ConnectionDetails] = JsonCodecMaker.make
  given codecLinkState: JsonValueCodec[OverlayConnectionDirectory.LinkState] = JsonCodecMaker.make
  given codecConnectedPeer: JsonValueCodec[OverlayConnectionDirectory.ConnectedPeer] = JsonCodecMaker.make
  given codecNodeInfo: JsonValueCodec[OverlayConnectionDirectory.NodeInfo] = JsonCodecMaker.make
  given codecReplicatedSetString: JsonValueCodec[ReplicatedSet[String]] = AWSetStateCodec[String]
  given codecReplicatedSetConnectionDetails: JsonValueCodec[ReplicatedSet[ConnectionDetails]] = AWSetStateCodec[ConnectionDetails]
  given codecReplicatedSetConnectedPeer: JsonValueCodec[ReplicatedSet[OverlayConnectionDirectory.ConnectedPeer]] =
    AWSetStateCodec[OverlayConnectionDirectory.ConnectedPeer]
  given codecDirectoryState: JsonValueCodec[ObserveRemoveMap[Uid, OverlayConnectionDirectory.NodeInfo]] =
    ORMapStateCodec[Uid, OverlayConnectionDirectory.NodeInfo]
  given codecDemoState: JsonValueCodec[DemoState] = JsonCodecMaker.make
  given codecOverlayEnvelope: JsonValueCodec[HyParViewMultiplexed.Envelope[DemoState, Set[ConnectionDetails]]] =
    HyParViewMultiplexed.envelopeCodec[DemoState, Set[ConnectionDetails]]
  given codecSignalSession: JsonValueCodec[WebRtcSignalingProtocol.Session] = JsonCodecMaker.make
  given codecSignalMessage: JsonValueCodec[Message] = JsonCodecMaker.make

  private type Envelope = HyParViewMultiplexed.Envelope[DemoState, Set[ConnectionDetails]]

  private enum EdgeKind {
    case ActiveOverlay, PassiveOverlay
  }

  private case class GraphEdge(from: Uid, to: Uid, kind: EdgeKind)
  private case class GraphNode(uid: Uid, label: String, details: String, var x: Double, var y: Double, var vx: Double, var vy: Double, highlighted: Boolean)

  @volatile private var viewerUid: Option[Uid] = None
  @volatile private var network: OverlayConnectionDirectory.Directory = OverlayConnectionDirectory.empty
  @volatile private var connectionInfoText: String = "waiting for seed url parameter"
  @volatile private var selfConnectionStringText: String = ""
  private var currentNode: Option[OverlayDemoNode] = None
  private var infoNode: dom.html.TextArea | Null = null
  private var selfConnectionNode: dom.html.TextArea | Null = null
  private val positions = mutable.Map.empty[Uid, (Double, Double, Double, Double)]

  private def refreshText(): Unit = {
    if infoNode != null then {
      infoNode.nn.value = connectionInfoText
      infoNode.nn.style.height = "auto"
      infoNode.nn.style.height = s"${infoNode.nn.scrollHeight}px"
    }
    if selfConnectionNode != null then {
      selfConnectionNode.nn.value = selfConnectionStringText
      selfConnectionNode.nn.style.height = "auto"
      selfConnectionNode.nn.style.height = s"${selfConnectionNode.nn.scrollHeight}px"
    }
  }

  private def parseConnectionString(str: String): ConnectionDetails = {
    val trimmed = str.trim
    if trimmed.startsWith("{") then readFromString[ConnectionDetails](trimmed)
    else readFromString[ConnectionDetails](String(Base64.getUrlDecoder.decode(trimmed), java.nio.charset.StandardCharsets.UTF_8))
  }

  private def encodeConnectionString(details: ConnectionDetails): String =
    Base64.getUrlEncoder.withoutPadding.encodeToString(writeToString(details).getBytes(java.nio.charset.StandardCharsets.UTF_8))

  private def urlParam(name: String): Option[String] = {
    val url = new dom.URL(window.location.href)
    Option(url.searchParams.get(name))
  }

  private def defaultSeedString: Option[String] = urlParam("seed")
  private def defaultSignalUrl: Option[String]  = urlParam("signal")
  private def defaultUidString: Option[String]  = urlParam("uid")

  private def describeSeed(details: ConnectionDetails): String = details match
    case ConnectionDetails.Tcp(host, port) => s"ws://$host:$port"
    case ConnectionDetails.WebSocket(url)  => url
    case ConnectionDetails.WebRtc(peerId)  => s"webrtc:$peerId"
    case other                             => ConnectionDetails.describe(other)

  private def renderConnectionInfo(directory: OverlayConnectionDirectory.Directory): String = {
    val localStatus = currentNode match
      case Some(node) =>
        val active  = node.activeView.toList.sortBy(Uid.unwrap).map(Uid.unwrap)
        val passive = node.passiveView.toList.sortBy(Uid.unwrap).map(Uid.unwrap)
        s"local hyparview\n  active: ${if active.nonEmpty then active.mkString(", ") else "-"}\n  passive: ${if passive.nonEmpty then passive.mkString(", ") else "-"}"
      case None =>
        "local hyparview\n  active: -\n  passive: -"

    val replicated = directory.entries.toList.sortBy((uid, _) => Uid.unwrap(uid)).map { (uid, info) =>
      val active  = info.peers.elements.filter(_.state == LinkState.Active).map(_.uid).toList.sortBy(Uid.unwrap).map(Uid.unwrap)
      val passive = info.peers.elements.filter(_.state == LinkState.Passive).map(_.uid).toList.sortBy(Uid.unwrap).map(Uid.unwrap)
      val label   = if viewerUid.contains(uid) then s"${Uid.unwrap(uid)} (you)" else Uid.unwrap(uid)
      s"$label\n  active: ${if active.nonEmpty then active.mkString(", ") else "-"}\n  passive: ${if passive.nonEmpty then passive.mkString(", ") else "-"}"
    }

    if replicated.nonEmpty then (localStatus +: replicated).mkString("\n")
    else s"$localStatus\nconnected, but no replicated overlay state yet"
  }

  private def envelopeConnection(latent: LatentConnection[MessageBuffer]): LatentConnection[Envelope] =
    LatentConnection.adapt[MessageBuffer, Envelope](
      mb => readFromArray[Envelope](mb.asArray),
      msg => ArrayMessageBuffer(writeToArray(msg)),
      "webrtc-envelope-json"
    )(latent)

  private def createRtcConnector(): WebRTCConnector =
    WebRTCConnector(new dom.RTCConfiguration {
      iceServers = js.Array[dom.RTCIceServer](new dom.RTCIceServer {
        urls = js.Array[String]("stun:stun.t-online.de:3478")
      })
    })

  private def createOverlayDataChannel(connector: WebRTCConnector): dom.RTCDataChannel =
    connector.peerConnection.createDataChannel(
      "overlay-webrtc",
      new dom.RTCDataChannelInit {
        negotiated = true
        id = 23
      }
    )

  private final class SignalingClient(url: String, node: OverlayDemoNode, onRegistered: () => Unit) {
    private val wsResolver = new WebSocketConnectionDetailsResolver[Message]
    private val abort      = Abort()
    private var connection: Option[Connection[Message]] = None
    private val outgoing = mutable.Map.empty[Uid, WebRTCConnector]

    private def send(message: Message): Unit =
      connection.foreach(_.send(message).run(_ => ()))

    private def handleIncomingOffer(from: Uid, session: WebRtcSignalingProtocol.Session): Unit = {
      val connector = createRtcConnector()
      val channel   = createOverlayDataChannel(connector)
      var sentAnswer = false
      node.addOverlayConnection(envelopeConnection(WebRTCConnection.openLatent(channel)))
      connector.updateRemoteDescription(SessionDescription(session.descType, session.sdp)).run {
        case Success(_) =>
          connector.lifecycle.run {
            case Success(overview) if !sentAnswer && overview.iceGatheringState == dom.RTCIceGatheringState.complete =>
              overview.localSession.foreach { answer =>
                sentAnswer = true
                send(Message.Answer(node.localUid.uid, from, WebRtcSignalingProtocol.Session(answer.descType, answer.sdp)))
              }
            case Success(_)   => ()
            case Failure(err) => err.printStackTrace()
          }
        case Failure(err) => err.printStackTrace()
      }
    }

    private def handleIncomingAnswer(from: Uid, session: WebRtcSignalingProtocol.Session): Unit =
      outgoing.remove(from).foreach { connector =>
        connector.updateRemoteDescription(SessionDescription(session.descType, session.sdp)).run {
          case Success(_)   => ()
          case Failure(err) => err.printStackTrace()
        }
      }

    def start(): Unit =
      wsResolver.connect(ConnectionDetails.WebSocket(url), s"webrtc-signaling-${Uid.unwrap(node.localUid.uid)}").foreach {
        _.prepare { conn =>
          {
            case Success(Message.Register(_)) => ()
            case Success(Message.Offer(from, to, session)) if to == node.localUid.uid => handleIncomingOffer(from, session)
            case Success(Message.Answer(from, to, session)) if to == node.localUid.uid => handleIncomingAnswer(from, session)
            case Success(_) => ()
            case Failure(_) => connection = None
          }
        }.runIn(abort) {
          case Success(conn) =>
            connection = Some(conn)
            send(Message.Register(node.localUid.uid))
            onRegistered()
          case Failure(err) => err.printStackTrace()
        }
      }

    def canConnect(details: ConnectionDetails): Boolean =
      details match
        case ConnectionDetails.WebRtc(peerId) => peerId != Uid.unwrap(node.localUid.uid) && connection.nonEmpty
        case _                                => false

    def connect(details: ConnectionDetails, label: String): Option[LatentConnection[Envelope]] = details match
      case ConnectionDetails.WebRtc(peerId) if connection.nonEmpty && peerId != Uid.unwrap(node.localUid.uid) =>
        val target = Uid.predefined(peerId)
        Some(new LatentConnection[Envelope] {
          override def prepare(receiver: Receive[Envelope]): Async[Abort, Connection[Envelope]] =
            Async.fromCallback { abort ?=>
              val connector = createRtcConnector()
              val channel   = createOverlayDataChannel(connector)
              var sentOffer = false
              outgoing.update(target, connector)
              envelopeConnection(WebRTCConnection.openLatent(channel)).prepare(receiver).runIn(abort) {
                case Success(conn) =>
                  Async.handler.succeed(conn)
                case Failure(err) =>
                  outgoing.remove(target)
                  Async.handler.fail(err)
              }
              connector.smartUpdateLocalDescription.run {
                case Success(_) =>
                  connector.lifecycle.run {
                    case Success(overview) if !sentOffer && overview.iceGatheringState == dom.RTCIceGatheringState.complete =>
                      overview.localSession match
                        case Some(offer) =>
                          sentOffer = true
                          send(Message.Offer(node.localUid.uid, target, WebRtcSignalingProtocol.Session(offer.descType, offer.sdp)))
                        case None =>
                          outgoing.remove(target)
                          Async.handler.fail(IllegalStateException("missing local webrtc offer after ice gathering completed"))
                    case Success(_)   => ()
                    case Failure(err) =>
                      outgoing.remove(target)
                      Async.handler.fail(err)
                  }
                case Failure(err) =>
                  outgoing.remove(target)
                  Async.handler.fail(err)
              }
            }
        })
      case _ => None
  }

  private def stopCurrentNode(): Unit = {
    currentNode.foreach(_.stop())
    currentNode = None
    network = OverlayConnectionDirectory.empty
    viewerUid = None
    selfConnectionStringText = ""
  }

  private def connect(seedConnectionString: Option[String]): Unit = {
    stopCurrentNode()
    val seedDetails = seedConnectionString.filter(_.nonEmpty).map(parseConnectionString)
    val localUid    = defaultUidString.filter(_.nonEmpty).map(value => LocalUid(Uid.predefined(value))).getOrElse(LocalUid.gen())
    val signalUrl   = defaultSignalUrl
    val selfDetails = signalUrl.map(_ => Set(ConnectionDetails.WebRtc(Uid.unwrap(localUid.uid)))).getOrElse(Set.empty)

    var signalingRef: Option[SignalingClient] = None
    val webRtcResolver = new ConnectionDetailsResolver[ConnectionDetails, Envelope] {
      override def canConnect(details: ConnectionDetails): Boolean =
        signalingRef.exists(_.canConnect(details))

      override def connect(details: ConnectionDetails, label: String): Option[LatentConnection[Envelope]] =
        signalingRef.flatMap(_.connect(details, label))
    }
    val wsResolver = new WebSocketConnectionDetailsResolver[Envelope]
    val resolver   = ConnectionDetailsResolver.many(ConnectionDetailsResolver.orElse(webRtcResolver, wsResolver))

    val node = new OverlayDemoNode(
      selfDetails = selfDetails,
      listenEnvelope = None,
      envelopeResolver = resolver,
      onStateChanged = state => {
        network = state.connections
        connectionInfoText = renderConnectionInfo(state.connections)
        refreshText()
      },
      printOverlayEventsToStdout = true,
      localUid = localUid,
    )

    val joinAfterSignal = () => seedDetails.foreach {
      case seed: ConnectionDetails.WebRtc =>
        connectionInfoText = s"${connectionInfoText}\nsignaling registered, trying webrtc seed ${describeSeed(seed)}"
        refreshText()
        node.joinSeed(seed)
      case _ => ()
    }
    signalingRef = signalUrl.map(url => new SignalingClient(url, node, joinAfterSignal))
    currentNode = Some(node)
    viewerUid = Some(node.localUid.uid)
    selfConnectionStringText = selfDetails.headOption match
      case Some(details) => encodeConnectionString(details)
      case None          => ""
    connectionInfoText = (seedDetails, signalUrl) match
      case (Some(seed: ConnectionDetails.WebRtc), Some(url)) => s"starting peer\nseed: ${describeSeed(seed)}\nsignaling: $url\nwaiting for signaling registration before join"
      case (Some(seed: ConnectionDetails.WebRtc), None)      => s"starting peer\nseed: ${describeSeed(seed)}\nmissing signaling server url (?signal=ws://... required for WebRTC seed)"
      case (Some(seed), Some(url))                           => s"starting peer\nseed: ${describeSeed(seed)}\nsignaling: $url"
      case (Some(seed), None)                                => s"starting peer\nseed: ${describeSeed(seed)}"
      case (None, Some(url))                                 => s"starting peer\nsignaling: $url"
      case (None, None)                                      => "starting peer"
    refreshText()
    signalingRef.foreach(_.start())
    node.start(seedDetails.filterNot(_.isInstanceOf[ConnectionDetails.WebRtc]).toList)
  }

  private def buildGraph(width: Double, height: Double): (Vector[GraphNode], Vector[GraphEdge]) = {
    val edges         = mutable.LinkedHashSet.empty[GraphEdge]
    val detailsByNode = mutable.LinkedHashMap.empty[Uid, String]

    network.entries.foreach { (uid, info) =>
      val activeCount  = info.peers.elements.count(_.state == LinkState.Active)
      val passiveCount = info.peers.elements.count(_.state == LinkState.Passive)
      detailsByNode.update(uid, s"active=$activeCount passive=$passiveCount")
      info.peers.elements.foreach { peer =>
        edges += GraphEdge(uid, peer.uid, peer.state match
          case LinkState.Active  => EdgeKind.ActiveOverlay
          case LinkState.Passive => EdgeKind.PassiveOverlay
        )
        detailsByNode.getOrElseUpdate(peer.uid, peer.state match
          case LinkState.Active  => "active peer"
          case LinkState.Passive => "passive peer"
        )
      }
    }

    val uids  = detailsByNode.keySet.toVector.distinct
    val known = uids.toSet
    positions.filterInPlace((uid, _) => known.contains(uid))
    val nodes = uids.zipWithIndex.map { (uid, idx) =>
      val angle    = idx.toDouble / math.max(1, uids.size) * math.Pi * 2
      val initialX = width / 2 + math.cos(angle) * math.min(width, height) * 0.32
      val initialY = height / 2 + math.sin(angle) * math.min(width, height) * 0.32
      val (x, y, vx, vy) = positions.getOrElse(uid, (initialX, initialY, 0.0, 0.0))
      GraphNode(
        uid = uid,
        label = if viewerUid.contains(uid) then "you" else Uid.unwrap(uid),
        details = detailsByNode.getOrElse(uid, ""),
        x = x,
        y = y,
        vx = vx,
        vy = vy,
        highlighted = viewerUid.contains(uid),
      )
    }
    (nodes, edges.toVector)
  }

  private def tick(nodes: Vector[GraphNode], edges: Vector[GraphEdge], width: Double, height: Double): Unit = {
    val byUid   = nodes.map(n => n.uid -> n).toMap
    val centerX = width / 2
    val centerY = height / 2
    val margin  = 50.0

    nodes.combinations(2).foreach {
      case Seq(a, b) =>
        val dx = b.x - a.x
        val dy = b.y - a.y
        val distanceSq = math.max(1.0, dx * dx + dy * dy)
        val distance = math.sqrt(distanceSq)
        val repulsion = 1800.0 / (distanceSq + 80.0)
        val fxRepel = dx * repulsion / math.max(1.0, distance)
        val fyRepel = dy * repulsion / math.max(1.0, distance)
        a.vx -= fxRepel
        a.vy -= fyRepel
        b.vx += fxRepel
        b.vy += fyRepel
      case _ => ()
    }

    edges.foreach {
      case GraphEdge(from, to, EdgeKind.ActiveOverlay) =>
        for
          a <- byUid.get(from)
          b <- byUid.get(to)
        do
          val dx = b.x - a.x
          val dy = b.y - a.y
          val distance = math.max(1.0, math.sqrt(dx * dx + dy * dy))
          val desired = 170.0
          val pull = (distance - desired) * 0.005
          val fx = dx * pull / distance
          val fy = dy * pull / distance
          a.vx += fx
          a.vy += fy
          b.vx -= fx
          b.vy -= fy
      case GraphEdge(_, _, EdgeKind.PassiveOverlay) => ()
    }

    nodes.foreach { node =>
      node.vx += (centerX - node.x) * 0.0015
      node.vy += (centerY - node.y) * 0.0015
      if node.x < margin then node.vx += (margin - node.x) * 0.03
      if node.x > width - margin then node.vx -= (node.x - (width - margin)) * 0.03
      if node.y < margin then node.vy += (margin - node.y) * 0.03
      if node.y > height - margin then node.vy -= (node.y - (height - margin)) * 0.03
      node.vx *= 0.88
      node.vy *= 0.88
      node.x += node.vx
      node.y += node.vy
      positions.update(node.uid, (node.x, node.y, node.vx, node.vy))
    }
  }

  private def syncCanvasResolution(canvas: HTMLCanvasElement, ctx: CanvasRenderingContext2D): (Double, Double) = {
    val dpr           = math.max(1.0, dom.window.devicePixelRatio)
    val logicalWidth  = math.max(300.0, canvas.clientWidth.toDouble)
    val logicalHeight = math.max(300.0, canvas.clientHeight.toDouble)
    val pixelWidth    = math.round(logicalWidth * dpr).toInt
    val pixelHeight   = math.round(logicalHeight * dpr).toInt
    if canvas.width != pixelWidth || canvas.height != pixelHeight then {
      canvas.width = pixelWidth
      canvas.height = pixelHeight
    }
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
    (logicalWidth, logicalHeight)
  }

  private def renderGraph(ctx: CanvasRenderingContext2D, width: Double, height: Double, nodes: Vector[GraphNode], edges: Vector[GraphEdge]): Unit = {
    ctx.fillStyle = "#0b1020"
    ctx.fillRect(0, 0, width, height)

    val byUid = nodes.map(n => n.uid -> n).toMap

    edges.foreach { edge =>
      edge.kind match
        case EdgeKind.ActiveOverlay =>
          ctx.strokeStyle = "rgba(96, 165, 250, 0.85)"
          ctx.setLineDash(js.Array())
          ctx.lineWidth = 2.0
        case EdgeKind.PassiveOverlay =>
          ctx.strokeStyle = "rgba(148, 163, 184, 0.55)"
          ctx.setLineDash(js.Array(6, 6))
          ctx.lineWidth = 1.25
      for
        a <- byUid.get(edge.from)
        b <- byUid.get(edge.to)
      do
        ctx.beginPath()
        ctx.moveTo(a.x, a.y)
        ctx.lineTo(b.x, b.y)
        ctx.stroke()
    }
    ctx.setLineDash(js.Array())

    nodes.foreach { node =>
      ctx.beginPath()
      ctx.fillStyle = if node.highlighted then "#fde68a" else "#60a5fa"
      ctx.arc(node.x, node.y, 8, 0, math.Pi * 2)
      ctx.fill()

      ctx.fillStyle = "#e2e8f0"
      ctx.font = "10px sans-serif"
      ctx.fillText(node.label.take(12), node.x + 10, node.y - 2)
      if node.details.nonEmpty then {
        ctx.fillStyle = "#94a3b8"
        ctx.font = "9px sans-serif"
        ctx.fillText(node.details.take(72), node.x + 10, node.y + 9)
      }
    }
  }

  @JSExportTopLevel("RunOverlayNetworkObserver")
  def runObserver(seedConnectionString: String, observerName: String, topic: String = "unused"): js.Promise[String] = {
    val done        = Promise[String]()
    val buffer      = mutable.ArrayBuffer.empty[String]
    val seedDetails = parseConnectionString(seedConnectionString)
    val seedUrl     = describeSeed(seedDetails)
    val localUid    = LocalUid.gen()
    val resolver    = ConnectionDetailsResolver.many(new WebSocketConnectionDetailsResolver[Envelope])
    val node = new OverlayDemoNode(
      selfDetails = Set.empty,
      listenEnvelope = None,
      envelopeResolver = resolver,
      onStateChanged = state => {
        val rendered = state.connections.entries.map { (uid, info) =>
          val active  = info.peers.elements.filter(_.state == LinkState.Active).map(_.uid).toList.sortBy(Uid.unwrap).map(Uid.unwrap)
          val passive = info.peers.elements.filter(_.state == LinkState.Passive).map(_.uid).toList.sortBy(Uid.unwrap).map(Uid.unwrap)
          s"${Uid.unwrap(uid)} -> active=[${active.mkString(",")}] passive=[${passive.mkString(",")}]"
        }.toList.sorted.mkString("; ")
        val line = s"observer=$observerName snapshot=$rendered"
        println(line)
        buffer += line
        if state.connections.keySet.nonEmpty then {
          done.trySuccess(buffer.mkString("\n"))
          ()
        }
      },
      printOverlayEventsToStdout = false,
      localUid = localUid,
    )

    val line = s"observer=$observerName connected url=$seedUrl uid=${Uid.unwrap(node.localUid.uid)}"
    println(line)
    buffer += line
    node.start(List(seedDetails))

    js.Dynamic.global.setTimeout(() => if !done.isCompleted then {
      done.trySuccess(buffer.mkString("\n"))
      ()
    }, 3000)
    done.future.toJSPromise
  }

  @JSExportTopLevel("OverlayNetworkGraph")
  def run(): Unit = {
    val container = document.getElementById("app")
    val infoOut = textarea(
      readonly := true,
      rows := 1,
      width := "100%",
      backgroundColor := "#0f172a",
      color := "#cbd5e1",
      padding := "0.5rem",
      border := "1px solid #334155",
      onfocus := { (ev: dom.FocusEvent) => ev.target.asInstanceOf[dom.html.TextArea].select() },
    ).render
    val selfConnectionOut = textarea(
      readonly := true,
      rows := 1,
      width := "100%",
      backgroundColor := "#0f172a",
      color := "#cbd5e1",
      padding := "0.5rem",
      border := "1px solid #334155",
      onfocus := { (ev: dom.FocusEvent) => ev.target.asInstanceOf[dom.html.TextArea].select() },
    ).render
    val graphCanvas = canvas(
      width := "100%",
      height := "72vh",
      display := "block",
      border := "1px solid #334155",
      backgroundColor := "#0b1020",
    ).render

    val root = div(
      padding := "1rem",
      color := "#e2e8f0",
      backgroundColor := "#020617",
      minHeight := "100vh",
      fontFamily := "sans-serif",
      display := "flex",
      flexDirection := "column",
      div(flex := "1 1 auto", minHeight := "78vh", width := "100%", graphCanvas),
      div(marginTop := "0.75rem", flex := "0 0 auto", selfConnectionOut),
      div(marginTop := "0.75rem", flex := "0 0 auto", infoOut),
    ).render

    container.replaceChildren(root)
    infoNode = infoOut
    selfConnectionNode = selfConnectionOut
    refreshText()

    try connect(defaultSeedString)
    catch
      case ex: Throwable =>
        connectionInfoText =
          s"failed to parse/connect seed\nexpected ?seed=<base64-connection-string> or ?seed=<raw-json-ConnectionDetails>\noptional: ?signal=<ws-url>&uid=<peer-uid>\n${ex.getClass.getSimpleName}: ${Option(ex.getMessage).getOrElse("")}"
        refreshText()

    val ctx = graphCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    def frame(): Unit = {
      val (width, height) = syncCanvasResolution(graphCanvas, ctx)
      val (nodes, edges)  = buildGraph(width, height)
      tick(nodes, edges, width, height)
      renderGraph(ctx, width, height, nodes, edges)
      dom.window.requestAnimationFrame((_: Double) => frame())
      ()
    }
    frame()
  }
}
