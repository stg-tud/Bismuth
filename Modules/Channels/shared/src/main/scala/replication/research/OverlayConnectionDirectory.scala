package replication.research

import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import replication.overlay.HyParViewMultiplexed

/** vibecoded. dont trust 😉 */

object OverlayConnectionDirectory {

  enum LinkState {
    case Active, Passive
  }

  case class ConnectedPeer[Details](uid: Uid, details: Details, state: LinkState)

  case class NodeInfo[Details](
      selfDetails: ReplicatedSet[Details],
      peers: ReplicatedSet[ConnectedPeer[Details]],
  )
  object NodeInfo {
    given [Details]: Bottom[NodeInfo[Details]]  = Bottom.derived
    given [Details]: Lattice[NodeInfo[Details]] = Lattice.derived
  }

  type Directory[Details] = ObserveRemoveMap[Uid, NodeInfo[Details]]

  def empty[Details]: Directory[Details] = ObserveRemoveMap.empty

  def emptyNodeInfo[Details]: NodeInfo[Details] =
    NodeInfo(ReplicatedSet.empty, ReplicatedSet.empty)

  def updateNode[Details](
      state: Directory[Details],
      node: Uid,
      selfDetails: Iterable[Details],
      peers: Iterable[ConnectedPeer[Details]],
  )(using LocalUid): Directory[Details] = {
    val current        = state.get(node).getOrElse(emptyNodeInfo)
    val desiredDetails = selfDetails.toSet
    val desiredPeers   = peers.toSet

    val detailsAfterRemove = {
      val toRemove = current.selfDetails.elements -- desiredDetails
      if toRemove.nonEmpty then current.selfDetails.merge(current.selfDetails.removeAll(toRemove))
      else current.selfDetails
    }
    val nextDetails = {
      val toAdd = desiredDetails -- detailsAfterRemove.elements
      if toAdd.nonEmpty then detailsAfterRemove.merge(detailsAfterRemove.addAll(toAdd))
      else detailsAfterRemove
    }

    val peersAfterRemove = {
      val toRemove = current.peers.elements -- desiredPeers
      if toRemove.nonEmpty then current.peers.merge(current.peers.removeAll(toRemove))
      else current.peers
    }
    val nextPeers = {
      val toAdd = desiredPeers -- peersAfterRemove.elements
      if toAdd.nonEmpty then peersAfterRemove.merge(peersAfterRemove.addAll(toAdd))
      else peersAfterRemove
    }

    val desired = NodeInfo(nextDetails, nextPeers)

    if desired == current && state.contains(node) then empty
    else state.update(node, desired)
  }

  def updateNodePeers[Details](
      state: Directory[Details],
      node: Uid,
      peers: Iterable[ConnectedPeer[Details]],
  )(using LocalUid): Directory[Details] = {
    val current = state.get(node).getOrElse(emptyNodeInfo)
    updateNode(state, node, current.selfDetails.elements, peers)
  }

  def updateNodeDetails[Details](
      state: Directory[Details],
      node: Uid,
      selfDetails: Iterable[Details],
  )(using LocalUid): Directory[Details] = {
    val current = state.get(node).getOrElse(emptyNodeInfo)
    updateNode(state, node, selfDetails, current.peers.elements)
  }

  def updateNodeFromOverlay[Details](
      state: Directory[Details],
      node: Uid,
      selfDetails: Iterable[Details],
      activePeers: Iterable[HyParViewMultiplexed.PeerRef[Details]],
      passivePeers: Iterable[HyParViewMultiplexed.PeerRef[Details]],
  )(using LocalUid): Directory[Details] =
    updateNode(
      state,
      node,
      selfDetails,
      activePeers.iterator.map(peer => ConnectedPeer(peer.uid, peer.details, LinkState.Active)).toSet ++
        passivePeers.iterator.map(peer => ConnectedPeer(peer.uid, peer.details, LinkState.Passive)).toSet
    )

  def knownPeers[Details](state: Directory[Details], selfUid: Uid): Set[HyParViewMultiplexed.PeerRef[Details]] =
    state.entries.iterator
      .filterNot((uid, _) => uid == selfUid)
      .flatMap { (uid, info) =>
        info.selfDetails.elements.toList.sortBy(_.toString).headOption.map(details => HyParViewMultiplexed.PeerRef(uid, details))
      }
      .toSet
}
