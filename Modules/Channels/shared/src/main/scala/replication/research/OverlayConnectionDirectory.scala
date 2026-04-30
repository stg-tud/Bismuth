package replication.research

import rdts.base.{LocalUid, Uid}
import rdts.datatypes.{ObserveRemoveMap, ReplicatedSet}
import replication.overlay.HyParViewMultiplexed

/** vibecoded. dont trust 😉 */

object OverlayConnectionDirectory {

  case class ConnectedPeer[Details](uid: Uid, details: Details)

  type Directory[Details] = ObserveRemoveMap[Uid, ReplicatedSet[ConnectedPeer[Details]]]

  def empty[Details]: Directory[Details] = ObserveRemoveMap.empty

  def updateNodePeers[Details](
      state: Directory[Details],
      node: Uid,
      peers: Iterable[ConnectedPeer[Details]],
  )(using LocalUid): Directory[Details] = {
    val desired = peers.toSet
    val current = state.get(node).getOrElse(ReplicatedSet.empty[ConnectedPeer[Details]])

    if desired.isEmpty then
      if state.contains(node) then state.remove(node) else empty
    else {
      val toRemove = current.elements -- desired
      val afterRemove =
        if toRemove.nonEmpty then current.merge(current.removeAll(toRemove))
        else current

      val toAdd = desired -- afterRemove.elements
      val next =
        if toAdd.nonEmpty then afterRemove.merge(afterRemove.addAll(toAdd))
        else afterRemove

      if next == current then empty
      else state.update(node, next)
    }
  }

  def updateNodePeersFromOverlay[Details](
      state: Directory[Details],
      node: Uid,
      peers: Iterable[HyParViewMultiplexed.PeerRef[Details]],
  )(using LocalUid): Directory[Details] =
    updateNodePeers(state, node, peers.iterator.map(peer => ConnectedPeer(peer.uid, peer.details)).toSet)
}
