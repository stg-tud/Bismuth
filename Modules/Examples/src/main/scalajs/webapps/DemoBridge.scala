package webapps

import channels.MessagingIO
import channels.broadcastchannel.BroadcastChannelPairConnector
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import org.scalajs.dom
import rdts.base.ReplicaId

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Connects a case study running in an iframe of the demo page (`Website/src/pages/demos`) to a relaying
  * [[MessagingIO]] on the demo page, which handles connections to other replicas (WebRTC) for all case studies.
  *
  * The demo page passes a random `bridge` name as url parameter to the iframe, both sides then connect via
  * a broadcast channel of that name.
  */
object DemoBridge {

  def channelName(bridge: String): String = s"bismuth demo bridge $bridge"

  /** The bridge name, if this page is embedded in the demo page. */
  def bridgeName: Option[String] =
    Option(new dom.URLSearchParams(dom.window.location.search).get("bridge")).filter(_.nonEmpty)

  /** Connect `io` to the demo page if embedded there. Returns false if not embedded. */
  def connect(io: MessagingIO[?]): Boolean = bridgeName match
      case None         => false
      case Some(bridge) =>
        io.addClientConnection(BroadcastChannelPairConnector.named(channelName(bridge)))
        true

  /** The connection UI of a case study: the WebRTC view, unless the demo page handles connections. */
  def connectionView(io: MessagingIO[?]): dom.Element =
    if connect(io) then dom.document.createElement("span")
    else WebRTCConnectionView(io).example().render

  /** Deltas are forwarded without being interpreted, so the relay works for any case study using plumtree. */
  final class RawJson(val bytes: Array[Byte])
  given JsonValueCodec[RawJson] = new JsonValueCodec[RawJson] {
    def decodeValue(in: JsonReader, default: RawJson): RawJson = RawJson(in.readRawValAsBytes())
    def encodeValue(x: RawJson, out: JsonWriter): Unit         = out.writeRawVal(x.bytes)
    def nullValue: RawJson                                     = RawJson("null".getBytes)
  }

  /** Runs on the demo page: relays between the iframe on `bridge` and WebRTC peers (whose connection UI is
    * rendered into `container`). Peers are only discovered among demo pages showing the same `app`.
    * Returns a function to stop the relay.
    */
  @JSExportTopLevel("DemoBridgeHost")
  def host(app: String, bridge: String, container: dom.Element): js.Function0[Unit] = {
    val relay = MessagingIO.usingPlumtree[RawJson](ReplicaId.gen(), _ => ())
    relay.addClientConnection(BroadcastChannelPairConnector.named(channelName(bridge)))
    val view = WebRTCConnectionView(relay, offerChannel = s"channels local broadcast for webrtc offers ($app)")
    container.replaceChildren(view.example().render)
    () => {
      relay.globalAbort.abort()
      container.replaceChildren()
    }
  }
}
