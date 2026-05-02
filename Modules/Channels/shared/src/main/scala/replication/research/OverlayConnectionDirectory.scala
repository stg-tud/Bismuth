package replication.research

import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import replication.overlay.HyParViewMultiplexed

/** vibecoded as part of the hyparview experiments */
object OverlayConnectionDirectory {

  enum LinkState {
    case Active, Passive
  }

  case class ConnectedPeer(uid: Uid, state: LinkState)

  case class NodeInfo(
      peers: ReplicatedSet[ConnectedPeer],
      eagerPeers: ReplicatedSet[Uid],
  )
  object NodeInfo {
    given Bottom[NodeInfo]  = Bottom.derived
    given Lattice[NodeInfo] = Lattice.derived
  }

  type Directory = ObserveRemoveMap[Uid, NodeInfo]

  val empty: Directory = ObserveRemoveMap.empty

  def emptyNodeInfo: NodeInfo =
    NodeInfo(ReplicatedSet.empty, ReplicatedSet.empty)

  def updateNode(
      state: Directory,
      node: Uid,
      peers: Iterable[ConnectedPeer],
      eagerPeers: Iterable[Uid],
  )(using LocalUid): Directory = {
    val current           = state.get(node).getOrElse(emptyNodeInfo)
    val desiredPeers      = peers.toSet
    val desiredEagerPeers = eagerPeers.toSet

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

    val eagerAfterRemove = {
      val toRemove = current.eagerPeers.elements -- desiredEagerPeers
      if toRemove.nonEmpty then current.eagerPeers.merge(current.eagerPeers.removeAll(toRemove))
      else current.eagerPeers
    }
    val nextEager = {
      val toAdd = desiredEagerPeers -- eagerAfterRemove.elements
      if toAdd.nonEmpty then eagerAfterRemove.merge(eagerAfterRemove.addAll(toAdd))
      else eagerAfterRemove
    }

    val desired = NodeInfo(nextPeers, nextEager)

    if desired == current && state.contains(node) then empty
    else state.update(node, desired)
  }

  def updateNodeFromOverlay(
      state: Directory,
      node: Uid,
      activePeers: Iterable[HyParViewMultiplexed.PeerRef],
      passivePeers: Iterable[HyParViewMultiplexed.PeerRef],
      eagerPeers: Iterable[Uid],
  )(using LocalUid): Directory =
    updateNode(
      state,
      node,
      activePeers.iterator.map(peer => ConnectedPeer(peer.uid, LinkState.Active)).toSet ++
        passivePeers.iterator.map(peer => ConnectedPeer(peer.uid, LinkState.Passive)).toSet,
      eagerPeers
    )

  def removePeerReferences(
      state: Directory,
      node: Uid,
      peer: Uid,
  )(using LocalUid): Directory = {
    val current        = state.get(node).getOrElse(emptyNodeInfo)
    val remainingPeers = current.peers.elements.filterNot(_.uid == peer)
    updateNode(state, node, remainingPeers, current.eagerPeers.elements.filterNot(_ == peer))
  }

  def removeConnectionBothDirections(
      state: Directory,
      left: Uid,
      right: Uid,
  )(using LocalUid): Directory = {
    val afterLeft  = state.merge(removePeerReferences(state, left, right))
    val afterRight = afterLeft.merge(removePeerReferences(afterLeft, right, left))
    afterRight
  }

  def removeNodeEverywhere(
      state: Directory,
      node: Uid,
  )(using LocalUid): Directory = {
    val withoutRefs = state.entries.foldLeft(state) { case (acc, (owner, _)) =>
      acc.merge(removePeerReferences(acc, owner, node))
    }
    withoutRefs.merge(withoutRefs.remove(node))
  }
}
