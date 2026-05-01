package webapps.ex2026niowebsocket

import channels.*
import channels.webnativewebsockets.WebSocketConnectionDetailsResolver
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, HTMLCanvasElement, document, window}
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import replication.JsoniterCodecs.{AWSetStateCodec, ORMapStateCodec, given}
import replication.overlay.HyParViewMultiplexed
import replication.research.{OverlayConnectionDirectory, OverlayDemoNode}
import replication.research.OverlayConnectionDirectory.LinkState
import replication.research.OverlayNetworkProtocol.DemoState
import scalatags.JsDom.all.*

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

/** vibecoded. dont trust 😉 */
object OverlayNetworkGraph {

  private val logs = mutable.ArrayBuffer.empty[String]

  private def log(msg: String): Unit = {
    val line = s"[overlay-graph] $msg"
    logs.append(line)
    while logs.size > 20 do {
      logs.remove(0)
      ()
    }
    println(line)
    refreshText()
  }

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

  private enum EdgeKind {
    case ActiveOverlay, PassiveOverlay
  }

  private case class GraphEdge(from: Uid, to: Uid, kind: EdgeKind)

  private case class GraphNode(
      uid: Uid,
      label: String,
      details: String,
      var x: Double,
      var y: Double,
      var vx: Double,
      var vy: Double,
      highlighted: Boolean,
      color: String,
  )

  private val placeholderViewerUid = LocalUid.gen().uid
  @volatile private var viewerUid: Uid = placeholderViewerUid
  @volatile private var network: OverlayConnectionDirectory.Directory[ConnectionDetails] = OverlayConnectionDirectory.empty
  @volatile private var statusText: String = "disconnected"
  @volatile private var connectedToSeed: Boolean = false
  @volatile private var currentSeed: Option[ConnectionDetails] = None
  private var currentNode: Option[OverlayDemoNode] = None
  private var statusNode: dom.Element | Null = null
  private var debugNode: dom.Element | Null  = null
  private val positions = mutable.Map.empty[Uid, (Double, Double, Double, Double)]

  private def refreshText(): Unit = {
    if statusNode != null then statusNode.nn.textContent = statusText
    if debugNode != null then debugNode.nn.textContent = logs.mkString("\n")
  }

  private def defaultSeedString: String = {
    val url = new dom.URL(window.location.href)
    Option(url.searchParams.get("seed")).getOrElse("{\"type\":\"Tcp\",\"host\":\"127.0.0.1\",\"port\":8080}")
  }

  private def websocketUrl(details: ConnectionDetails): String = details match
    case ConnectionDetails.Tcp(host, port) => s"ws://$host:$port"
    case ConnectionDetails.WebSocket(url)  => url
    case other                             => throw IllegalArgumentException(s"unsupported seed details for web app: $other")

  private def describe(details: ConnectionDetails): String = details match
    case ConnectionDetails.Tcp(host, port)       => s"tcp:$host:$port"
    case ConnectionDetails.WebSocket(url)        => s"ws:$url"
    case ConnectionDetails.WebRtc(signaling, id) => s"webrtc:$id@$signaling"
    case ConnectionDetails.QueuedLocal(id)       => s"queued:$id"
    case ConnectionDetails.SynchronousLocal(id)  => s"sync:$id"

  private def stopCurrentNode(): Unit = {
    currentNode.foreach(_.stop())
    currentNode = None
    connectedToSeed = false
    network = OverlayConnectionDirectory.empty
  }

  private def connect(seedConnectionString: String): Unit = {
    stopCurrentNode()
    val seedDetails = readFromString[ConnectionDetails](seedConnectionString)
    val seedUrl     = websocketUrl(seedDetails)
    val localId     = LocalUid.gen()
    val membershipDetail = ConnectionDetails.WebRtc("browser", Uid.unwrap(localId.uid))
    val resolver         = new WebSocketConnectionDetailsResolver[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]]

    viewerUid = localId.uid
    currentSeed = Some(seedDetails)
    statusText = s"connecting to $seedUrl"
    refreshText()
    log(s"connect requested seed=$seedConnectionString url=$seedUrl")

    val node = new OverlayDemoNode(
      selfDetails = Set.empty,
      membershipDetails = Some(membershipDetail),
      listenEnvelope = None,
      envelopeResolver = resolver,
      onDirectoryChanged = directory => {
        network = directory
        connectedToSeed = true
        val active  = directory.get(viewerUid).map(_.peers.elements.count(_.state == LinkState.Active)).getOrElse(0)
        val passive = directory.get(viewerUid).map(_.peers.elements.count(_.state == LinkState.Passive)).getOrElse(0)
        statusText = s"connected • ${directory.keySet.size} known nodes • active=$active passive=$passive"
        refreshText()
      },
    )

    currentNode = Some(node)
    viewerUid = node.localUid.uid
    node.start(List(seedDetails))
    log(s"overlay node started uid=${Uid.unwrap(viewerUid)} seed=$seedUrl")
  }

  private def buildGraph(width: Double, height: Double): (Vector[GraphNode], Vector[GraphEdge]) = {
    val edges         = mutable.LinkedHashSet.empty[GraphEdge]
    val detailsByNode = mutable.LinkedHashMap.empty[Uid, String]
    val colors        = mutable.LinkedHashMap.empty[Uid, String]

    detailsByNode.update(viewerUid, currentSeed.map(describe).map(seed => s"you • seed=$seed").getOrElse("you"))
    colors.update(viewerUid, "#f59e0b")

    network.entries.foreach { (uid, info) =>
      val selfLabel = info.selfDetails.elements.toList.sortBy(_.toString).map(describe).mkString(", ")
      detailsByNode.getOrElseUpdate(uid, if selfLabel.nonEmpty then selfLabel else uid.show)
      colors.getOrElseUpdate(uid, if uid == viewerUid then "#f59e0b" else "#60a5fa")
      info.peers.elements.foreach { peer =>
        edges += GraphEdge(uid, peer.uid, peer.state match
          case LinkState.Active  => EdgeKind.ActiveOverlay
          case LinkState.Passive => EdgeKind.PassiveOverlay
        )
        detailsByNode.getOrElseUpdate(peer.uid, s"${describe(peer.details)} • ${peer.state}")
        colors.getOrElseUpdate(peer.uid, peer.state match
          case LinkState.Active  => "#60a5fa"
          case LinkState.Passive => "#94a3b8"
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
        label = if uid == viewerUid then "you" else Uid.unwrap(uid),
        details = detailsByNode.getOrElse(uid, uid.show),
        x = x,
        y = y,
        vx = vx,
        vy = vy,
        highlighted = uid == viewerUid,
        color = colors.getOrElse(uid, "#60a5fa"),
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
        val distanceSq = math.max(200.0, dx * dx + dy * dy)
        val force = 350.0 / distanceSq
        val fx = dx * force / math.sqrt(distanceSq)
        val fy = dy * force / math.sqrt(distanceSq)
        a.vx -= fx
        a.vy -= fy
        b.vx += fx
        b.vy += fy
      case _ => ()
    }

    edges.foreach { edge =>
      for
        a <- byUid.get(edge.from)
        b <- byUid.get(edge.to)
      do
        val dx = b.x - a.x
        val dy = b.y - a.y
        val distance = math.max(1.0, math.sqrt(dx * dx + dy * dy))
        val desired = edge.kind match
          case EdgeKind.ActiveOverlay  => 120.0
          case EdgeKind.PassiveOverlay => 180.0
        val stiffness = edge.kind match
          case EdgeKind.ActiveOverlay  => 0.012
          case EdgeKind.PassiveOverlay => 0.004
        val pull = (distance - desired) * stiffness
        val fx = dx * pull / distance
        val fy = dy * pull / distance
        a.vx += fx
        a.vy += fy
        b.vx -= fx
        b.vy -= fy
    }

    nodes.foreach { node =>
      if node.uid == viewerUid && !connectedToSeed then {
        node.x = centerX
        node.y = centerY
        node.vx = 0
        node.vy = 0
      } else {
        node.vx += (centerX - node.x) * 0.004
        node.vy += (centerY - node.y) * 0.004

        if node.x < margin then node.vx += (margin - node.x) * 0.03
        if node.x > width - margin then node.vx -= (node.x - (width - margin)) * 0.03
        if node.y < margin then node.vy += (margin - node.y) * 0.03
        if node.y > height - margin then node.vy -= (node.y - (height - margin)) * 0.03

        node.vx *= 0.82
        node.vy *= 0.82
        node.x += node.vx
        node.y += node.vy
      }

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
      ctx.fillStyle = if node.highlighted then "#fde68a" else node.color
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
    val seedDetails = readFromString[ConnectionDetails](seedConnectionString)
    val seedUrl     = websocketUrl(seedDetails)
    val localId     = LocalUid.gen()
    val resolver    = new WebSocketConnectionDetailsResolver[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]]
    val node = new OverlayDemoNode(
      selfDetails = Set.empty,
      membershipDetails = Some(ConnectionDetails.WebRtc("browser", Uid.unwrap(localId.uid))),
      listenEnvelope = None,
      envelopeResolver = resolver,
      onDirectoryChanged = directory => {
        val rendered = directory.entries.map { (uid, info) =>
          s"${Uid.unwrap(uid)} -> [${info.peers.elements.map(p => Uid.unwrap(p.uid)).toList.sorted.mkString(",")}]"
        }.toList.sorted.mkString("; ")
        val line = s"observer=$observerName snapshot=$rendered"
        println(line)
        buffer += line
        if directory.keySet.size >= 2 || directory.entries.exists((_, info) => info.peers.elements.nonEmpty) then
          done.trySuccess(buffer.mkString("\n"))
          ()
      },
    )

    val line = s"observer=$observerName connected url=$seedUrl uid=${Uid.unwrap(localId.uid)}"
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

    val urlInput = input(
      placeholder := "seed connection string",
      value := defaultSeedString,
      width := "100%",
      backgroundColor := "#fff",
      color := "#111827",
      padding := "0.5rem",
      border := "1px solid #94a3b8",
    ).render
    val connectButton = button("Connect").render
    val status = div(style := "user-select: text;").render
    val debugOut = pre(
      style := "user-select: text;",
      backgroundColor := "#0f172a",
      color := "#cbd5e1",
      padding := "0.75rem",
      maxHeight := "12rem",
      overflowY := "auto",
      whiteSpace := "pre-wrap",
    ).render
    val graphCanvas = canvas(
      width := "100%",
      height := "72vh",
      display := "block",
      border := "1px solid #334155",
      backgroundColor := "#0b1020",
    ).render

    connectButton.onclick = _ => connect(urlInput.value)

    val root = div(
      padding := "1rem",
      color := "#e2e8f0",
      backgroundColor := "#020617",
      minHeight := "100vh",
      fontFamily := "sans-serif",
      div(marginBottom := "0.75rem", urlInput, connectButton),
      status,
      div(marginTop := "1rem", width := "100%", graphCanvas),
      div(marginTop := "0.75rem", debugOut),
    ).render

    container.replaceChildren(root)

    val ctx = graphCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    def frame(): Unit = {
      val (width, height) = syncCanvasResolution(graphCanvas, ctx)
      val (nodes, edges)  = buildGraph(width, height)
      tick(nodes, edges, width, height)
      renderGraph(ctx, width, height, nodes, edges)
      dom.window.requestAnimationFrame((_: Double) => frame())
      ()
    }

    statusNode = status
    debugNode = debugOut
    refreshText()
    frame()
  }
}
