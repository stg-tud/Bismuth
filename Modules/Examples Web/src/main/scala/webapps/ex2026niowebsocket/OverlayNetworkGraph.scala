package webapps.ex2026niowebsocket

import channels.*
import channels.webnativewebsockets.WebSocketConnectionDetailsResolver
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, HTMLCanvasElement, document, window}
import rdts.base.{Uid, LocalUid}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import replication.JsoniterCodecs.{AWSetStateCodec, ORMapStateCodec, given}
import replication.overlay.HyParViewMultiplexed
import replication.research.{OverlayConnectionDirectory, OverlayDemoNode}
import replication.research.OverlayConnectionDirectory.LinkState
import replication.research.OverlayNetworkProtocol.DemoState
import scalatags.JsDom.all.*

import java.util.Base64
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

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

  private enum EdgeKind {
    case ActiveOverlay, PassiveOverlay
  }

  private case class GraphEdge(from: Uid, to: Uid, kind: EdgeKind)
  private case class GraphNode(uid: Uid, label: String, details: String, var x: Double, var y: Double, var vx: Double, var vy: Double, highlighted: Boolean)

  private val placeholderViewerUid = LocalUid.gen().uid
  @volatile private var viewerUid: Uid = placeholderViewerUid
  @volatile private var network: OverlayConnectionDirectory.Directory = OverlayConnectionDirectory.empty
  @volatile private var connectionInfoText: String = "waiting for seed url parameter"
  private var currentNode: Option[OverlayDemoNode] = None
  private var infoNode: dom.html.TextArea | Null = null
  private val positions = mutable.Map.empty[Uid, (Double, Double, Double, Double)]

  private def refreshText(): Unit =
    if infoNode != null then {
      infoNode.nn.value = connectionInfoText
      infoNode.nn.style.height = "auto"
      infoNode.nn.style.height = s"${infoNode.nn.scrollHeight}px"
    }

  private def parseConnectionString(str: String): ConnectionDetails =
    readFromString[ConnectionDetails](String(Base64.getUrlDecoder.decode(str), java.nio.charset.StandardCharsets.UTF_8))

  private def defaultSeedString: Option[String] = {
    val url = new dom.URL(window.location.href)
    Option(url.searchParams.get("seed"))
  }

  private def websocketUrl(details: ConnectionDetails): String = details match
    case ConnectionDetails.Tcp(host, port) => s"ws://$host:$port"
    case ConnectionDetails.WebSocket(url)  => url
    case other                             => throw IllegalArgumentException(s"unsupported seed details for web app: $other")

  private def renderConnectionInfo(directory: OverlayConnectionDirectory.Directory): String = {
    val lines = directory.entries.toList.sortBy((uid, _) => Uid.unwrap(uid)).map { (uid, info) =>
      val active  = info.peers.elements.filter(_.state == LinkState.Active).map(_.uid).toList.sortBy(Uid.unwrap).map(Uid.unwrap)
      val passive = info.peers.elements.filter(_.state == LinkState.Passive).map(_.uid).toList.sortBy(Uid.unwrap).map(Uid.unwrap)
      val label   = if uid == viewerUid then s"${Uid.unwrap(uid)} (you)" else Uid.unwrap(uid)
      s"$label\n  active: ${if active.nonEmpty then active.mkString(", ") else "-"}\n  passive: ${if passive.nonEmpty then passive.mkString(", ") else "-"}"
    }
    if lines.nonEmpty then lines.mkString("\n") else "connected, but no replicated overlay state yet"
  }

  private def stopCurrentNode(): Unit = {
    currentNode.foreach(_.stop())
    currentNode = None
    network = OverlayConnectionDirectory.empty
  }

  private def connect(seedConnectionString: String): Unit = {
    stopCurrentNode()
    val seedDetails = parseConnectionString(seedConnectionString)
    val resolver    = ConnectionDetailsResolver.many(new WebSocketConnectionDetailsResolver[HyParViewMultiplexed.Envelope[DemoState, Set[ConnectionDetails]]])
    val node = new OverlayDemoNode(
      selfDetails = Set.empty,
      listenEnvelope = None,
      envelopeResolver = resolver,
      onStateChanged = state => {
        network = state.connections
        connectionInfoText = renderConnectionInfo(state.connections)
        refreshText()
      },
      printOverlayEventsToStdout = false,
    )

    currentNode = Some(node)
    viewerUid = node.localUid.uid
    connectionInfoText = s"connecting to ${websocketUrl(seedDetails)}"
    refreshText()
    node.start(List(seedDetails))
  }

  private def buildGraph(width: Double, height: Double): (Vector[GraphNode], Vector[GraphEdge]) = {
    val edges         = mutable.LinkedHashSet.empty[GraphEdge]
    val detailsByNode = mutable.LinkedHashMap.empty[Uid, String]

    network.entries.foreach { (uid, info) =>
      detailsByNode.getOrElseUpdate(uid, if uid == viewerUid then "you" else uid.show)
      info.peers.elements.foreach { peer =>
        edges += GraphEdge(uid, peer.uid, peer.state match
          case LinkState.Active  => EdgeKind.ActiveOverlay
          case LinkState.Passive => EdgeKind.PassiveOverlay
        )
        detailsByNode.getOrElseUpdate(peer.uid, peer.uid.show)
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
        label = if uid == viewerUid then "you" else Uid.unwrap(uid),
        details = detailsByNode.getOrElse(uid, uid.show),
        x = x,
        y = y,
        vx = vx,
        vy = vy,
        highlighted = uid == viewerUid,
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
      ctx.fillStyle = "#94a3b8"
      ctx.font = "9px sans-serif"
      ctx.fillText(node.details.take(72), node.x + 10, node.y + 9)
    }
  }

  @JSExportTopLevel("RunOverlayNetworkObserver")
  def runObserver(seedConnectionString: String, observerName: String, topic: String = "unused"): js.Promise[String] = {
    val done   = Promise[String]()
    val buffer = mutable.ArrayBuffer.empty[String]
    val seedDetails = parseConnectionString(seedConnectionString)
    val seedUrl     = websocketUrl(seedDetails)
    val resolver    = ConnectionDetailsResolver.many(new WebSocketConnectionDetailsResolver[HyParViewMultiplexed.Envelope[DemoState, Set[ConnectionDetails]]])
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
        if state.connections.keySet.nonEmpty then
          done.trySuccess(buffer.mkString("\n"))
          ()
      },
      printOverlayEventsToStdout = false,
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
      div(flex := "1 1 auto", minHeight := "70vh", width := "100%", graphCanvas),
      div(marginTop := "0.75rem", flex := "0 0 auto", infoOut),
    ).render

    container.replaceChildren(root)
    infoNode = infoOut
    refreshText()

    defaultSeedString match
      case Some(seed) =>
        connect(seed)
      case None =>
        connectionInfoText = "missing ?seed=<connection-string> in the url"
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
