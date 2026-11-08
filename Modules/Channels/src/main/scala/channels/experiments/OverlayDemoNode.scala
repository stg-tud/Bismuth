package channels.experiments

import channels.JsoniterCodecs.given
import channels.broadcast.PlumtreeBroadcast
import channels.broadcast.PlumtreeMessage.Payload
import channels.connection.{Abort, ChannelResolver, ConnectionDescriptor, Connector, Listener, Peer, PeerConnectInfo}
import channels.overlay.HyParViewStateMachine
import channels.overlay.HyParViewStateMachine.HyParViewConfig
import channels.{MessagingIO, MergingHistory, Timers}
import rdts.base.Lattice.syntax
import rdts.base.{Bottom, Lattice, ReplicaId, Uid}

import java.util.{Timer, TimerTask}
import scala.util.Random

class OverlayDemoNode(
    selfDetails: Set[ConnectionDescriptor],
    listenEnvelope: Option[Listener],
    envelopeResolver: ChannelResolver,
    random: Random = Random(0),
    config: HyParViewConfig = HyParViewConfig.fromEstimatedNetworkSize(10),
    onStateChanged: OverlayStatusProtocol.Status => Unit = _ => (),
    printOverlayEventsToStdout: Boolean = false,
    runBackgroundTasks: Boolean = true,
    val localUid: ReplicaId = ReplicaId.gen(),
)(using Lattice[Payload[OverlayStatusProtocol.Status]]) {

  @volatile var state: OverlayStatusProtocol.Status = OverlayStatusProtocol.empty

  private val selfRef = PeerConnectInfo(Peer.Claimed(localUid.uid), selfDetails)
  private val abort   = Abort()
  private val timer   = Timer(true)
  private var broadcastIO: Option[MessagingIO.Plumtree[OverlayStatusProtocol.Status]] = None

  private def nowMillis(): Long = System.currentTimeMillis()

  private def emitStateChanged(): Unit =
    onStateChanged(state)

  private def publish(delta: OverlayStatusProtocol.Status): Unit =
    if !Bottom.isEmpty(delta) then
        state = state.merge(delta)
        emitStateChanged()
        broadcastIO.foreach(_.broadcast(delta))

  private def newOverlay() =
    MessagingIO[OverlayStatusProtocol.Status](
      localUid,
      payload => {
        state = state.merge(payload.data)
        emitStateChanged()
      },
      overlay = Some(HyParViewStateMachine.empty(selfRef, config, random.between, _ => true)),
      resolver = envelopeResolver,
      globalAbort = abort,
      broadcast =
        Some(PlumtreeBroadcast(
          deltaStorage = MergingHistory[OverlayStatusProtocol.Status](blockSize = 1000)
        )),
      // overlay maintenance and broadcast repair run on their own timers
      timers = if runBackgroundTasks then Timers.OnJavaTimer(timer) else Timers.Manual(),
    )

  private def startBackgroundTasks(): Unit =
    timer.schedule(
      new TimerTask {
        override def run(): Unit = overlayInfoTick()
      },
      1000L,
      1000L,
    )

  def start(seeds: List[ConnectionDescriptor] = Nil): Unit = {
    val node = newOverlay()
    broadcastIO = Some(node)
    listenEnvelope.foreach(node.addServerConnection)
    seeds.foreach(detail => bootstrapVia(detail))
    overlayInfoTick()
    if runBackgroundTasks then startBackgroundTasks()
  }

  def bootstrapVia(peer: ConnectionDescriptor): Unit =
    broadcastIO.foreach(_.bootstrapVia(peer))

  def selfConnectionDetails: Set[ConnectionDescriptor] = selfDetails

  def overlayInfoTick(): Unit =
    broadcastIO.foreach { io =>
      given ReplicaId = localUid
      publish(OverlayStatusProtocol.statusDelta(state, io, nowMillis()))
    }

  def addOverlayConnection(latent: Connector): Unit =
    broadcastIO.foreach(_.addClientConnection(latent))

  def activeView: Set[Uid] = broadcastIO.collect {
    case io if io.overlayController.isInstanceOf[HyParViewStateMachine] =>
      io.overlayController.asInstanceOf[HyParViewStateMachine].activeView.map(_.uid)
  }.getOrElse(Set.empty)

  def passiveView: Set[Uid] = broadcastIO.collect {
    case io if io.overlayController.isInstanceOf[HyParViewStateMachine] =>
      io.overlayController.asInstanceOf[HyParViewStateMachine].passiveView.map(_.uid)
  }.getOrElse(Set.empty)

  def eagerView: Set[Uid] = broadcastIO.map(_.state.eagerPeers.map(_.uid)).getOrElse(Set.empty)

  def lastIncomingMessageTimes: Map[Uid, Long] = Map.empty

  def stop(): Unit = {
    abort.abort()
    timer.cancel()
  }
}
