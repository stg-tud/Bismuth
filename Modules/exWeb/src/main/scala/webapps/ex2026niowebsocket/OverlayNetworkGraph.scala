package webapps.ex2026niowebsocket

import channels.*
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, document, window}
import rdts.base.{LocalUid, Uid}
import replication.JsoniterCodecs.given
import replication.research.{OverlayConnectionDirectory, OverlayDemoNode}
import scalatags.JsDom.all.*
import webapps.ex2026niowebsocket.OverlayNetworkGraphModel.LocalViews
import webapps.ex2026niowebsocket.OverlayNetworkGraphNetworking.WebRtcSignalingBridge

import java.util.Base64
import scala.collection.mutable
import scala.scalajs.js.annotation.JSExportTopLevel

/** vibecoded as part of the hyparview experiments */
object OverlayNetworkGraph {

  @volatile private var viewerUid: Option[Uid]                        = None
  @volatile private var network: OverlayConnectionDirectory.Directory = OverlayConnectionDirectory.empty
  @volatile private var connectionInfoText: String                    = "waiting for seed url parameter"
  @volatile private var selfConnectionStringText: String              = ""
  private var currentNode: Option[OverlayDemoNode]                    = None
  private var currentSignalingBridge: Option[WebRtcSignalingBridge]   = None
  private var infoNode: dom.html.TextArea | Null                      = null
  private var selfConnectionNode: dom.html.TextArea | Null            = null
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

  private def parseConnectionString(str: String): ChannelConnectInfo = {
    val trimmed = str.trim
    if trimmed.startsWith("{") then readFromString[ChannelConnectInfo](trimmed)
    else
        readFromString[ChannelConnectInfo](String(
          Base64.getUrlDecoder.decode(trimmed),
          java.nio.charset.StandardCharsets.UTF_8
        ))
  }

  private def encodeConnectionString(details: ChannelConnectInfo): String =
    Base64.getUrlEncoder.withoutPadding.encodeToString(
      writeToString(details).getBytes(java.nio.charset.StandardCharsets.UTF_8)
    )

  private def urlParam(name: String): Option[String] = {
    val url = new dom.URL(window.location.href)
    Option(url.searchParams.get(name))
  }

  private def defaultSeedString: Option[String] = urlParam("seed")
  private def defaultSignalUrl: Option[String]  = urlParam("signal")
  private def defaultUidString: Option[String]  = urlParam("uid")

  private def describeSeed(details: ChannelConnectInfo): String = details.toString

  private def localViews: Option[LocalViews] =
    currentNode.map(node =>
      LocalViews(node.activeView, node.passiveView, node.eagerView, node.lastIncomingMessageTimes)
    )

  private def updateDisplayedState(directory: OverlayConnectionDirectory.Directory): Unit = {
    network = directory
    connectionInfoText = OverlayNetworkGraphModel.renderConnectionInfo(directory, viewerUid, localViews)
    refreshText()
  }

  private def stopCurrentNode(): Unit = {
    currentSignalingBridge.foreach(_.stop())
    currentSignalingBridge = None
    currentNode.foreach(_.stop())
    currentNode = None
    network = OverlayConnectionDirectory.empty
    viewerUid = None
    selfConnectionStringText = ""
  }

  private def connect(seedConnectionString: Option[String]): Unit = {
    stopCurrentNode()
    val seedDetails = seedConnectionString.filter(_.nonEmpty).map(parseConnectionString)
    val localUid    =
      defaultUidString.filter(_.nonEmpty).map(value => LocalUid(Uid.predefined(value))).getOrElse(LocalUid.gen())
    val signalUrl      = defaultSignalUrl
    val signalingTopic = "overlay-demo"
    val selfDetails    =
      signalUrl.map(_ => Set(ChannelConnectInfo.WebRtc(Uid.unwrap(localUid.uid)))).getOrElse(Set.empty)
    val requestedSeedId = seedDetails.collect { case ChannelConnectInfo.WebRtc(peerId) => Uid.predefined(peerId) }

    var signalingRef: Option[WebRtcSignalingBridge] = None
    val resolver                                    = new ChannelResolver {
      override def canConnect(details: ChannelConnectInfo): Boolean =
        signalingRef.exists(_.canConnect(details))

      override def connect(details: ChannelConnectInfo): Option[LatentConnection] =
        signalingRef.flatMap(_.connect(details))
    }

    val node = new OverlayDemoNode(
      selfDetails = selfDetails,
      listenEnvelope = None,
      envelopeResolver = resolver,
      onStateChanged = state => updateDisplayedState(state.connections),
      printOverlayEventsToStdout = true,
      localUid = localUid,
    )

    val joinAfterSignal = () =>
      requestedSeedId.foreach { seedUid =>
        connectionInfoText = s"${connectionInfoText}\nsignaling registered, discovering seed ${Uid.unwrap(seedUid)}"
        refreshText()
      }

    signalingRef = signalUrl.map(url =>
      new WebRtcSignalingBridge(url, signalingTopic, node, selfDetails, requestedSeedId, 3, joinAfterSignal)
    )
    currentSignalingBridge = signalingRef
    currentNode = Some(node)
    viewerUid = Some(node.localUid.uid)
    selfConnectionStringText = selfDetails.headOption.map(encodeConnectionString).getOrElse("")

    connectionInfoText = (seedDetails, signalUrl) match
        case (Some(seed: ChannelConnectInfo.WebRtc), Some(url)) =>
          s"starting peer\nseed: ${describeSeed(seed)}\nsignaling: $url\nwaiting for signaling registration before discovery"
        case (Some(seed: ChannelConnectInfo.WebRtc), None) =>
          s"starting peer\nseed: ${describeSeed(seed)}\nmissing signaling server url (?signal=ws://... required for WebRTC seed discovery)"
        case (Some(seed), _) =>
          s"starting peer\nseed: ${describeSeed(seed)}\nweb app only acquires peers via signaling + WebRTC; direct seed connection is ignored"
        case (None, Some(url)) =>
          s"starting peer\nsignaling: $url"
        case (None, None) =>
          "starting peer\nmissing signaling server url (?signal=ws://...)"

    refreshText()
    signalingRef.foreach(_.start())
    node.start(Nil)
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
            s"failed to parse/connect seed\nexpected ?seed=<base64-connection-string> or ?seed=<raw-json-ConnectionDetails>\nrequired: ?signal=<ws-url>\noptional: ?uid=<peer-uid>\n${ex.getClass.getSimpleName}: ${Option(ex.getMessage).getOrElse("")}"
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
