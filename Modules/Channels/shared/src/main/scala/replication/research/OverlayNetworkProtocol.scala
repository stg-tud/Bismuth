package replication.research

import channels.ChannelConnectDescriptor
import rdts.base.{Bottom, Lattice}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}

/** vibecoded as part of the hyparview experiments */
object OverlayNetworkProtocol {

  type TopicRegistry  = ObserveRemoveMap[String, ReplicatedSet[ChannelConnectDescriptor]]
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
}
