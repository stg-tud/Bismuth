package replication.research

import channels.ConnectionDetails
import rdts.base.{Bottom, Lattice, Uid}
import rdts.datatypes.{LastWriterWins, ObserveRemoveMap, ReplicatedSet}

/** vibecoded. dont trust 😉 */

object OverlayNetworkProtocol {

  type TopicRegistry = ObserveRemoveMap[String, ReplicatedSet[ConnectionDetails]]
  type NetworkByTopic = ObserveRemoveMap[String, OverlayConnectionDirectory.Directory[ConnectionDetails]]

  case class WebRtcOffer(id: String, sdpType: String, sdp: String)
  case class WebRtcAnswer(offerId: String, sdpType: String, sdp: String)
  type WebRtcOffers = ObserveRemoveMap[Uid, ObserveRemoveMap[Uid, LastWriterWins[Option[WebRtcOffer]]]]
  type WebRtcAnswers = ObserveRemoveMap[Uid, ObserveRemoveMap[Uid, LastWriterWins[Option[WebRtcAnswer]]]]

  case class DemoState(
      values: ReplicatedSet[String],
      connections: OverlayConnectionDirectory.Directory[ConnectionDetails],
      webRtcOffers: WebRtcOffers,
      webRtcAnswers: WebRtcAnswers,
  )
  object DemoState {
    given Bottom[DemoState]  = Bottom.derived
    given Lattice[DemoState] = Lattice.derived

    val empty: DemoState = DemoState(ReplicatedSet.empty, OverlayConnectionDirectory.empty, ObserveRemoveMap.empty, ObserveRemoveMap.empty)
  }

  enum CoordinationMessage {
    case Register(topic: String, details: ConnectionDetails)
    case Unregister(topic: String, details: ConnectionDetails)
    case Observe(topic: String)
    case Unobserve(topic: String)
    case ReportConnections(topic: String, node: Uid, peers: ReplicatedSet[OverlayConnectionDirectory.ConnectedPeer[ConnectionDetails]])
    case RemoveNode(topic: String, node: Uid)
    case Snapshot(state: TopicRegistry)
    case NetworkSnapshot(topic: String, state: OverlayConnectionDirectory.Directory[ConnectionDetails])
  }
}
