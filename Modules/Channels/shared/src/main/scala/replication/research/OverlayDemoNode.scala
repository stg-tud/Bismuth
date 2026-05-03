package replication.research

import channels.{Abort, ChannelConnectDescriptor, ChannelResolver, LatentConnection}
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
    runBackgroundTasks: Boolean = true,
    val localUid: LocalUid = LocalUid.gen(),
) {

  @volatile var state: DemoState = DemoState.empty

  private val selfRef = HyParViewMultiplexed.PeerRef(localUid.uid, selfDetails)
  private val abort   = Abort()
  private val timer   = Timer(true)
  private var overlay: Option[replication.overlay.HyParViewMultiplexedNode[DemoState]] = None
  private var mismatchChecksInRow                                                      = 0

  private val heartbeatIntervalMillis = 10_000L
  private val staleNodeAfterMillis    = 30_000L

  private def nowMillis(): Long = System.currentTimeMillis()

  private def emitStateChanged(): Unit =
    onStateChanged(state)

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
      nowMillis(),
    )
    if !Bottom.isEmpty(delta) then
        if replicate then publish(DemoState(ReplicatedSet.empty, delta))
        else {
          state = state.merge(DemoState(ReplicatedSet.empty, delta))
          emitStateChanged()
        }
  }

  private def publishLocalView(): Unit = refreshLocalView(replicate = true)

  private def expectedLocalView: OverlayConnectionDirectory.ViewSnapshot =
    OverlayConnectionDirectory.ViewSnapshot(
      active = overlay.map(_.activeView).getOrElse(Set.empty),
      passive = overlay.map(_.passiveView).getOrElse(Set.empty),
      eager = overlay.map(_.eagerView).getOrElse(Set.empty),
    ).normalized

  private def checkReplicatedLocalView(): Unit = {
    val expected   = expectedLocalView
    val replicated = OverlayConnectionDirectory.snapshot(state.connections, localUid.uid)
    if replicated == expected then mismatchChecksInRow = 0
    else {
      mismatchChecksInRow += 1
      if mismatchChecksInRow >= 3 then
          Console.err.println(
            s"[overlay-state-mismatch ${Uid.unwrap(localUid.uid)}] local(active=${expected.active.map(Uid.unwrap).toList.sorted.mkString(",")}; passive=${expected.passive.map(Uid.unwrap).toList.sorted.mkString(",")}; eager=${expected.eager.map(Uid.unwrap).toList.sorted.mkString(",")}) != replicated(active=${replicated.active.map(Uid.unwrap).toList.sorted.mkString(",")}; passive=${replicated.passive.map(Uid.unwrap).toList.sorted.mkString(",")}; eager=${replicated.eager.map(Uid.unwrap).toList.sorted.mkString(",")})"
          )
    }
  }

  private def removeDisconnectedConnection(peer: Uid): Unit = {
    given LocalUid = localUid
    val delta      = OverlayConnectionDirectory.removeConnectionBothDirections(state.connections, localUid.uid, peer)
    if !Bottom.isEmpty(delta) then publish(DemoState(ReplicatedSet.empty, delta))
  }

  private def pruneStaleReplicatedNodes(): Unit = {
    given LocalUid = localUid
    val delta      = OverlayConnectionDirectory.pruneStaleNodes(
      state.connections,
      nowMillis(),
      staleNodeAfterMillis,
      localUid.uid,
    )
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

  private def startBackgroundTasks(): Unit = {
    timer.schedule(
      new TimerTask {
        override def run(): Unit = {
          overlay.foreach(_.promotionTick())
          overlay.foreach(_.shuffleTick())
          overlay.foreach(_.repairTick())
        }
      },
      1000L,
      1000L,
    )
    timer.schedule(
      new TimerTask {
        override def run(): Unit = {
          publishLocalView()
          pruneStaleReplicatedNodes()
        }
      },
      heartbeatIntervalMillis,
      heartbeatIntervalMillis,
    )
    timer.schedule(
      new TimerTask {
        override def run(): Unit = checkReplicatedLocalView()
      },
      1000L,
      1000L,
    )
  }

  def start(seeds: List[ChannelConnectDescriptor] = Nil): Unit = {
    val node = newOverlay(seeds.headOption)
    overlay = Some(node)
    listenEnvelope.foreach(_ => node.startServer())
    publishLocalView()
    if runBackgroundTasks then startBackgroundTasks()
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

  def shuffleTick(): Unit = {
    overlay.foreach(_.promotionTick())
    overlay.foreach(_.shuffleTick())
    overlay.foreach(_.repairTick())
    publishLocalView()
  }

  def repairTick(): Unit = overlay.foreach(_.repairTick())

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
