package replication.research

import channels.{Abort, ChannelConnectDescriptor, ChannelResolver, LatentConnection}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.base.Lattice.syntax
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.ReplicatedSet
import replication.StateDeltaStorage
import replication.overlay.HyParViewMultiplexed
import replication.overlay.HyParViewUnified.HyParViewConfig
import replication.research.OverlayNetworkProtocol.DemoState

import java.util.{Timer, TimerTask}
import scala.util.Random

/** vibecoded as part of the hyparview experiments */
class OverlayDemoNode(
                       selfDetails: Set[ChannelConnectDescriptor],
                       listenEnvelope: Option[LatentConnection[HyParViewMultiplexed.Envelope[DemoState]]],
                       envelopeResolver: ChannelResolver[HyParViewMultiplexed.Envelope[DemoState]],
                       random: Random = Random(0),
                       config: HyParViewConfig = HyParViewConfig.fromEstimatedNetworkSize(10),
                       onStateChanged: DemoState => Unit = _ => (),
                       printOverlayEventsToStdout: Boolean = false,
                       val localUid: LocalUid = LocalUid.gen(),
)(using JsonValueCodec[DemoState]) {

  @volatile var state: DemoState = DemoState.empty

  private val selfRef = HyParViewMultiplexed.PeerRef(localUid.uid, selfDetails)
  private val abort   = Abort()
  private val timer   = Timer(true)
  private var overlay: Option[replication.overlay.HyParViewMultiplexedNode[DemoState]] = None

  private def emitStateChanged(): Unit = onStateChanged(state)

  private def publish(delta: DemoState): Unit =
    if !Bottom.isEmpty(delta) then
        state = state.merge(delta)
        emitStateChanged()
        overlay.foreach(_.applyDelta(delta))

  private def refreshLocalView(replicate: Boolean): Unit = {
    given LocalUid = localUid
    val delta      = OverlayConnectionDirectory.updateNodeFromOverlay(
      state.connections,
      localUid.uid,
      overlay.map(_.activePeers).getOrElse(Set.empty),
      overlay.map(_.passivePeers).getOrElse(Set.empty),
      overlay.map(_.eagerView).getOrElse(Set.empty),
    )
    if !Bottom.isEmpty(delta) then
      if replicate then publish(DemoState(ReplicatedSet.empty, delta))
      else {
        state = state.merge(DemoState(ReplicatedSet.empty, delta))
        emitStateChanged()
      }
  }

  private def publishLocalView(): Unit = refreshLocalView(replicate = true)

  private def removeDisconnectedConnection(peer: Uid): Unit = {
    given LocalUid = localUid
    val delta      = OverlayConnectionDirectory.removeConnectionBothDirections(state.connections, localUid.uid, peer)
    if !Bottom.isEmpty(delta) then publish(DemoState(ReplicatedSet.empty, delta))
  }

  private def newOverlay(seed: Option[ChannelConnectDescriptor]) =
    new replication.overlay.HyParViewMultiplexedNode[DemoState](
      selfRef,
      (delta: DemoState) => {
        state = state.merge(delta)
        emitStateChanged()
      },
      listenEnvelope.getOrElse(new LatentConnection[HyParViewMultiplexed.Envelope[DemoState]] {
        override def prepare(receiver: channels.Receive[HyParViewMultiplexed.Envelope[DemoState]]) =
          throw UnsupportedOperationException("no local server configured for this overlay node")
      }),
      envelopeResolver,
      seed.map(Set(_)),
      random,
      StateDeltaStorage(() => state),
      config,
      onViewChanged = (_, _) => publishLocalView(),
      onPeerRolesChanged = () => refreshLocalView(replicate = false),
      onPeerDisconnected = removeDisconnectedConnection,
    )

  private def startShuffleTask(): Unit =
    timer.schedule(
      new TimerTask {
        override def run(): Unit = overlay.foreach(_.shuffleTick())
      },
      1000L,
      1000L,
    )

  def start(seeds: List[ChannelConnectDescriptor] = Nil): Unit = {
    val node = newOverlay(seeds.headOption)
    overlay = Some(node)
    listenEnvelope.foreach(_ => node.startServer())
    publishLocalView()
    startShuffleTask()
    seeds.headOption.foreach(_ => node.join())
  }

  def joinSeed(seed: ChannelConnectDescriptor): Unit =
    overlay.foreach(_.join(Set(seed)))

  def discoverPeers(peers: Iterable[replication.overlay.HyParViewMultiplexed.PeerRef]): Unit =
    overlay.foreach(_.discoverPeers(peers))

  def selfConnectionDetails: Set[ChannelConnectDescriptor] = selfDetails

  def publishAdd(value: String): Unit = {
    given LocalUid = localUid
    publish(DemoState(state.values.add(value), OverlayConnectionDirectory.empty))
    publishLocalView()
  }

  def publishRemove(value: String): Unit = {
    publish(DemoState(state.values.remove(value), OverlayConnectionDirectory.empty))
    publishLocalView()
  }

  def shuffleTick(): Unit = overlay.foreach(_.shuffleTick())

  def addOverlayConnection(latent: LatentConnection[HyParViewMultiplexed.Envelope[DemoState]]): Unit =
    overlay.foreach(_.addIncomingConnection(latent))

  def activeView: Set[Uid] = overlay.map(_.activeView).getOrElse(Set.empty)

  def passiveView: Set[Uid] = overlay.map(_.passiveView).getOrElse(Set.empty)

  def eagerView: Set[Uid] = overlay.map(_.eagerView).getOrElse(Set.empty)

  def connectionDirectory: OverlayConnectionDirectory.Directory = state.connections

  def stop(): Unit = {
    given LocalUid = localUid
    val cleanup    = OverlayConnectionDirectory.removeNodeEverywhere(state.connections, localUid.uid)
    if !Bottom.isEmpty(cleanup) then {
      state = state.merge(DemoState(ReplicatedSet.empty, cleanup))
      emitStateChanged()
      overlay.foreach(_.applyDelta(DemoState(ReplicatedSet.empty, cleanup)))
    }
    overlay.foreach(_.stop())
    abort.abort()
    timer.cancel()
  }
}
