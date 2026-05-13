package webapps.ex2026overlaydemo

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, document, window}
import rdts.base.{LocalUid, Uid}
import replication.JsoniterCodecs.given
import replication.research.{OverlayDemoNode, OverlayStatusProtocol}
import scalatags.JsDom.all.*
import webapps.ex2026overlaydemo.OverlayNetworkGraphModel.LocalViews
import webapps.ex2026overlaydemo.OverlayNetworkGraphNetworking.WebRtcSignalingBridge

import java.util.Base64
import scala.collection.mutable
import scala.scalajs.js.annotation.JSExportTopLevel

/** vibecoded as part of the hyparview experiments */
object OverlayNetworkGraph {

  @volatile private var viewerUid: Option[Uid]                      = None
  @volatile private var network: OverlayStatusProtocol.Status       = OverlayStatusProtocol.empty
  @volatile private var connectionInfoText: String                  = "waiting for seed url parameter"
  @volatile private var selfConnectionStringText: String            = ""
  private var currentNode: Option[OverlayDemoNode]                  = None
  private var currentSignalingBridge: Option[WebRtcSignalingBridge] = None
  private var infoNode: dom.html.TextArea | Null                    = null
  private var selfConnectionNode: dom.html.TextArea | Null          = null
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

  private def parseConnectionString(str: String): ConnectionDescriptor = {
    val trimmed = str.trim
    ConnectionDescriptor.parse(trimmed)
      .orElse(Option.when(trimmed.startsWith("{"))(readFromString[ConnectionDescriptor](trimmed)))
      .orElse {
        scala.util.Try(
          readFromString[ConnectionDescriptor](String(
            Base64.getUrlDecoder.decode(trimmed),
            java.nio.charset.StandardCharsets.UTF_8
          ))
        ).toOption
      }
      .getOrElse(throw IllegalArgumentException(s"Invalid connection string: $trimmed"))
  }

  private def encodeConnectionString(details: ConnectionDescriptor): String =
    Base64.getUrlEncoder.withoutPadding.encodeToString(
      writeToString(details).getBytes(java.nio.charset.StandardCharsets.UTF_8)
    )

  private def urlParam(name: String): Option[String] = {
    val url = new dom.URL(window.location.href)
    Option(url.searchParams.get(name))
  }

  private def defaultSeedString: Option[String] = urlParam("bootstrap").orElse(urlParam("seed"))
  private def defaultSignalUrl: Option[String]  = urlParam("signal")
  private def defaultUidString: Option[String]  = urlParam("uid")

  private def describeSeed(details: ConnectionDescriptor): String = details.toString

  private def localViews: Option[LocalViews] =
    currentNode.map(node =>
      LocalViews(node.activeView, node.passiveView, node.eagerView, node.lastIncomingMessageTimes)
    )

  private def updateDisplayedState(status: OverlayStatusProtocol.Status): Unit = {
    network = status
    connectionInfoText = OverlayNetworkGraphModel.renderConnectionInfo(status, viewerUid, localViews)
    refreshText()
  }

  private def stopCurrentNode(): Unit = {
    currentSignalingBridge.foreach(_.stop())
    currentSignalingBridge = None
    currentNode = None
    network = OverlayStatusProtocol.empty
    viewerUid = None
    selfConnectionStringText = ""
  }

  private def connect(seedConnectionString: Option[String]): Unit = {
    stopCurrentNode()
    val seedDetails = seedConnectionString.filter(_.nonEmpty).map(parseConnectionString)
    val localUid    =
      defaultUidString.filter(_.nonEmpty).map(value => LocalUid(Uid.predefined(value))).getOrElse(LocalUid.gen())
    val signalDetails = defaultSignalUrl.filter(_.nonEmpty).map(parseConnectionString)
    val selfDetails   =
      signalDetails.map(_ => Set(ConnectionDescriptor.WebRtc(Uid.unwrap(localUid.uid)))).getOrElse(Set.empty)
    val requestedSeedId = seedDetails.collect { case ConnectionDescriptor.WebRtc(peerId) => Uid.predefined(peerId) }

    var signalingRef: Option[WebRtcSignalingBridge] = None
    val wsResolver = new channels.webnativewebsockets.WebSocketConnectionDetailsResolver
    val resolver   = new ChannelResolver {
      override def connect(details: ConnectionDescriptor): Option[LatentConnection[Connection]] =
        wsResolver.connect(details).orElse(signalingRef.flatMap(_.connect(details)))
    }

    val node = new OverlayDemoNode(
      selfDetails = selfDetails,
      listenEnvelope = None,
      envelopeResolver = resolver,
      onStateChanged = updateDisplayedState,
      printOverlayEventsToStdout = true,
      localUid = localUid,
    )

    var nodeStarted                             = false
    def startNodeAndBootstrapDirectSeed(): Unit =
      if !nodeStarted then {
        node.start(Nil)
        seedDetails match
            case Some(seed) if !seed.isInstanceOf[ConnectionDescriptor.WebRtc] =>
              node.bootstrapVia(seed)
            case _ => ()
        nodeStarted = true
      }

    val joinAfterSignal = () => {
      startNodeAndBootstrapDirectSeed()
      requestedSeedId.foreach { seedUid =>
        connectionInfoText =
          s"${connectionInfoText}\nsignaling registered, bootstrapping via webrtc://${Uid.unwrap(seedUid)}"
        refreshText()
      }
    }

    signalingRef = signalDetails.map(details =>
      new WebRtcSignalingBridge(details, "overlay-demo", node, selfDetails, requestedSeedId, joinAfterSignal)
    )
    currentSignalingBridge = signalingRef
    currentNode = Some(node)
    viewerUid = Some(node.localUid.uid)
    selfConnectionStringText = selfDetails.headOption.map(encodeConnectionString).getOrElse("")

    connectionInfoText = (seedDetails, signalDetails) match
        case (Some(seed: ConnectionDescriptor.WebRtc), Some(signal)) =>
          s"starting peer\nbootstrap: ${describeSeed(seed)}\nsignaling: ${signal.asUrl}\nwaiting for signaling registration before WebRTC bootstrap"
        case (Some(seed: ConnectionDescriptor.WebRtc), None) =>
          s"starting peer\nbootstrap: ${describeSeed(seed)}\nmissing signaling server (?signal=ws://... or ?signal=tcp+ws://... required for webrtc:// bootstrap)"
        case (Some(seed), Some(signal)) =>
          s"starting peer\nbootstrap: ${describeSeed(seed)}\nsignaling: ${signal.asUrl}"
        case (Some(seed), None) =>
          s"starting peer\nbootstrap: ${describeSeed(seed)}"
        case (None, Some(signal)) =>
          s"starting peer\nsignaling: ${signal.asUrl}"
        case (None, None) =>
          "starting peer\nmissing bootstrap seed (?bootstrap=...) or signaling server (?signal=ws://... or tcp+ws://...)"

    refreshText()
    signalingRef match
        case Some(bridge) =>
          bridge.start()
        case None =>
          startNodeAndBootstrapDirectSeed()
  }

  @JSExportTopLevel("OverlayNetworkGraph")
  def run(): Unit = {
    val container = document.getElementById("app")
    val infoOut   = textarea(
      readonly        := true,
      rows            := 1,
      width           := "100%",
      backgroundColor := "#0f172a",
      color           := "#cbd5e1",
      padding         := "0.5rem",
      border          := "1px solid #334155",
      onfocus         := { (ev: dom.FocusEvent) => ev.target.asInstanceOf[dom.html.TextArea].select() },
    ).render
    val selfConnectionOut = textarea(
      readonly        := true,
      rows            := 1,
      width           := "100%",
      backgroundColor := "#0f172a",
      color           := "#cbd5e1",
      padding         := "0.5rem",
      border          := "1px solid #334155",
      onfocus         := { (ev: dom.FocusEvent) => ev.target.asInstanceOf[dom.html.TextArea].select() },
    ).render
    val graphCanvas = canvas(
      width           := "100%",
      height          := "72vh",
      display         := "block",
      border          := "1px solid #334155",
      backgroundColor := "#0b1020",
    ).render

    val root = div(
      padding         := "1rem",
      color           := "#e2e8f0",
      backgroundColor := "#020617",
      minHeight       := "100vh",
      fontFamily      := "sans-serif",
      display         := "flex",
      flexDirection   := "column",
      div(flex      := "1 1 auto", minHeight := "78vh", width := "100%", graphCanvas),
      div(marginTop := "0.75rem", flex       := "0 0 auto", selfConnectionOut),
      div(marginTop := "0.75rem", flex       := "0 0 auto", infoOut),
    ).render

    container.replaceChildren(root)
    infoNode = infoOut
    selfConnectionNode = selfConnectionOut
    refreshText()

    try connect(defaultSeedString)
    catch
        case ex: Throwable =>
          connectionInfoText =
            s"failed to parse/connect seed\nexpected ?bootstrap=<tcp-ws://host:port|webrtc://peer|base64|raw-json>\noptional: ?signal=<ws://...|tcp-ws://host:port>\noptional: ?uid=<peer-uid>\n${ex.getClass.getSimpleName}: ${Option(ex.getMessage).getOrElse("")}"
          refreshText()

    val ctx           = graphCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    def frame(): Unit = {
      val (width, height) = OverlayNetworkGraphRendering.syncCanvasResolution(graphCanvas, ctx)
      val (nodes, edges) = OverlayNetworkGraphModel.buildGraph(network, viewerUid, localViews, positions, width, height)
      OverlayNetworkGraphModel.tick(nodes, edges, positions, width, height)
      OverlayNetworkGraphRendering.renderGraph(ctx, width, height, nodes, edges)
      dom.window.requestAnimationFrame((_: Double) => frame())
      ()
    }
    frame()
  }
}
