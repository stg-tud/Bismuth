package replication.research

import channels.ConnectionDetails
import rdts.base.{Bottom, Lattice, Uid}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}

/** vibecoded as part of the hyparview experiments */
object OverlayNetworkProtocol {

  type TopicRegistry  = ObserveRemoveMap[String, ReplicatedSet[ConnectionDetails]]
  type NetworkByTopic = ObserveRemoveMap[String, OverlayConnectionDirectory.Directory]

  case class DemoState(
      values: ReplicatedSet[String],
      connections: OverlayConnectionDirectory.Directory,
  )
  object DemoState {
    given Bottom[DemoState]  = Bottom.derived
    given Lattice[DemoState] = Lattice.derived

    val empty: DemoState = DemoState(ReplicatedSet.empty, OverlayConnectionDirectory.empty)
  }

  enum CoordinationMessage {
    case Register(topic: String, details: ConnectionDetails)
    case Unregister(topic: String, details: ConnectionDetails)
    case Observe(topic: String)
    case Unobserve(topic: String)
    case ReportConnections(topic: String, node: Uid, peers: ReplicatedSet[OverlayConnectionDirectory.ConnectedPeer])
    case RemoveNode(topic: String, node: Uid)
    case Snapshot(state: TopicRegistry)
    case NetworkSnapshot(topic: String, state: OverlayConnectionDirectory.Directory)
  }
}
