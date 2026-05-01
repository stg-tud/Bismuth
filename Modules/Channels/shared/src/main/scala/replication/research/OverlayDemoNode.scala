package replication.research

import channels.{Abort, ConnectionDetails, ConnectionDetailsResolver, LatentConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.ReplicatedSet
import replication.PlumtreeDissemination
import replication.overlay.{HyParViewMultiplexed, HyParViewMultiplexedNode, HyParViewUnified}
import replication.research.OverlayNetworkProtocol.DemoState

import java.util.{Timer, TimerTask}
import scala.util.Random

/** vibecoded. dont trust 😉 */
class OverlayDemoNode(
    selfDetails: Set[ConnectionDetails],
    membershipDetails: Option[ConnectionDetails],
    listenEnvelope: Option[LatentConnection[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]]],
    envelopeResolver: ConnectionDetailsResolver[ConnectionDetails, HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]],
    random: Random = Random(0),
    config: HyParViewUnified.HyParViewConfig = HyParViewUnified.HyParViewConfig.fromEstimatedNetworkSize(10),
    onDirectoryChanged: OverlayConnectionDirectory.Directory[ConnectionDetails] => Unit = _ => (),
)(using JsonValueCodec[DemoState]) {

  val localUid: LocalUid = LocalUid.gen()

  @volatile var state: DemoState = DemoState.empty

  private val selfRef = HyParViewMultiplexed.PeerRef(localUid.uid, membershipDetails.orElse(selfDetails.headOption).getOrElse(
    throw IllegalArgumentException("OverlayDemoNode requires membership details or at least one self connection detail")
  ))
  private var overlay: Option[HyParViewMultiplexedNode[DemoState, ConnectionDetails]] = None
  private val shuffleTimer = Timer(true)
  private val abort        = Abort()

  private def refreshOverlayKnowledge(): Unit = {
    onDirectoryChanged(state.connections)
    overlay.foreach(_.discoverPeers(OverlayConnectionDirectory.knownPeers(state.connections, localUid.uid)))
  }

  val plumtree: PlumtreeDissemination[DemoState] = PlumtreeDissemination(
    localUid,
    delta => {
      state = state.merge(delta)
      refreshOverlayKnowledge()
    },
    None,
    globalAbort = abort,
  )

  private def publishLocalView(
      activePeers: Set[HyParViewMultiplexed.PeerRef[ConnectionDetails]],
      passivePeers: Set[HyParViewMultiplexed.PeerRef[ConnectionDetails]],
  ): Unit = {
    given LocalUid = localUid
    val delta = OverlayConnectionDirectory.updateNodeFromOverlay(
      state.connections,
      localUid.uid,
      selfDetails,
      activePeers,
      passivePeers,
    )
    if !Bottom.isEmpty(delta) then {
      val wrapped = DemoState(ReplicatedSet.empty, delta)
      state = state.merge(wrapped)
      refreshOverlayKnowledge()
      plumtree.applyDelta(wrapped)
    }
  }

  private def handleDisconnectedPeer(peer: Uid): Unit = {
    given LocalUid = localUid
    val delta = OverlayConnectionDirectory.removeConnectionBothDirections(state.connections, localUid.uid, peer)
    if !Bottom.isEmpty(delta) then {
      val wrapped = DemoState(ReplicatedSet.empty, delta)
      state = state.merge(wrapped)
      refreshOverlayKnowledge()
      plumtree.applyDelta(wrapped)
    }
  }

  private def newOverlay(contact: Option[ConnectionDetails]): HyParViewMultiplexedNode[DemoState, ConnectionDetails] =
    new HyParViewMultiplexedNode(
      selfRef,
      plumtree,
      listenEnvelope.getOrElse(new LatentConnection[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]] {
        override def prepare(receiver: channels.Receive[HyParViewMultiplexed.Envelope[DemoState, ConnectionDetails]]) =
          throw UnsupportedOperationException("no local server configured for this overlay node")
      }),
      envelopeResolver,
      contact,
      random,
      config,
      onViewChanged = publishLocalView,
      onPeerDisconnected = handleDisconnectedPeer,
    )

  private def startShuffleTask(): Unit =
    shuffleTimer.schedule(
      new TimerTask {
        override def run(): Unit = {
          overlay.foreach(_.shuffleTick())
          refreshOverlayKnowledge()
        }
      },
      1000L,
      1000L,
    )

  def start(seeds: List[ConnectionDetails] = Nil): Unit = {
    val node = newOverlay(seeds.headOption)
    overlay = Some(node)
    listenEnvelope.foreach(_ => node.startServer())
    startShuffleTask()
    publishLocalView(Set.empty, Set.empty)
    seeds.headOption.foreach(_ => node.join())
  }

  def publishAdd(value: String): Unit = {
    given LocalUid = localUid
    val delta   = state.values.add(value)
    val wrapped = DemoState(delta, OverlayConnectionDirectory.empty)
    state = state.merge(wrapped)
    plumtree.applyDelta(wrapped)
  }

  def publishRemove(value: String): Unit = {
    val delta   = state.values.remove(value)
    val wrapped = DemoState(delta, OverlayConnectionDirectory.empty)
    state = state.merge(wrapped)
    plumtree.applyDelta(wrapped)
  }

  def activeView: Set[Uid] = overlay.map(_.activeView).getOrElse(Set.empty)

  def passiveView: Set[Uid] = overlay.map(_.passiveView).getOrElse(Set.empty)

  def connectionDirectory: OverlayConnectionDirectory.Directory[ConnectionDetails] = state.connections

  def stop(): Unit = {
    given LocalUid = localUid
    val cleanup = OverlayConnectionDirectory.removeNodeEverywhere(state.connections, localUid.uid)
    if !Bottom.isEmpty(cleanup) then {
      val wrapped = DemoState(ReplicatedSet.empty, cleanup)
      state = state.merge(wrapped)
      refreshOverlayKnowledge()
      plumtree.applyDelta(wrapped)
    }
    abort.abort()
    shuffleTimer.cancel()
  }
}
