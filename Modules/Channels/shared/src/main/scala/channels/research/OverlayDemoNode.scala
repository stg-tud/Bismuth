package channels.research

import channels.JsoniterCodecs.given
import channels.broadcast.PlumtreeBroadcast
import channels.broadcast.PlumtreeMessage.Payload
import channels.connection.{Abort, ChannelResolver, Connection, ConnectionDescriptor, LatentConnection, PeerConnectInfo}
import channels.overlay.HyParViewStateMachine
import channels.overlay.HyParViewStateMachine.HyParViewConfig
import channels.{BroadcastIO, MergingHistory}
import rdts.base.Lattice.syntax
import rdts.base.{Bottom, Lattice, LocalUid, Uid}

import java.util.{Timer, TimerTask}
import scala.util.Random

class OverlayDemoNode(
    selfDetails: Set[ConnectionDescriptor],
    listenEnvelope: Option[LatentConnection[ConnectionDescriptor]],
    envelopeResolver: ChannelResolver,
    random: Random = Random(0),
    config: HyParViewConfig = HyParViewConfig.fromEstimatedNetworkSize(10),
    onStateChanged: OverlayStatusProtocol.Status => Unit = _ => (),
    printOverlayEventsToStdout: Boolean = false,
    runBackgroundTasks: Boolean = true,
    val localUid: LocalUid = LocalUid.gen(),
)(using Lattice[Payload[OverlayStatusProtocol.Status]]) {

  @volatile var state: OverlayStatusProtocol.Status = OverlayStatusProtocol.empty

  private val selfRef = PeerConnectInfo(localUid.uid, selfDetails)
  private val abort   = Abort()
  private val timer   = Timer(true)
  private var broadcastIO: Option[BroadcastIO[OverlayStatusProtocol.Status]] = None

  private def nowMillis(): Long = System.currentTimeMillis()

  private def emitStateChanged(): Unit =
    onStateChanged(state)

  private def publish(delta: OverlayStatusProtocol.Status): Unit =
    if !Bottom.isEmpty(delta) then
        state = state.merge(delta)
        emitStateChanged()
        broadcastIO.foreach(_.broadcast(delta))

  private def newOverlay() =
    BroadcastIO[OverlayStatusProtocol.Status](
      localUid,
      delta => {
        state = state.merge(delta)
        emitStateChanged()
      },
      overlay = Some(HyParViewStateMachine.empty(selfRef, config, random.between, _ => true)),
      resolver = envelopeResolver,
      globalAbort = abort,
      broadcast =
        Some(PlumtreeBroadcast(
          localUid.uid,
          deltaStorage = MergingHistory[OverlayStatusProtocol.Status](blockSize = 1000)
        ))
    )

  private def startBackgroundTasks(): Unit = {
    timer.schedule(
      new TimerTask {
        override def run(): Unit =
          repairTick()
      },
      5000L,
      5000L,
    )
    timer.schedule(
      new TimerTask {
        override def run(): Unit = overlayInfoTick()
      },
      1000L,
      1000L,
    )
  }

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

  def discoverPeers(peers: Iterable[PeerConnectInfo]): Unit =
    broadcastIO.foreach(_.discover(peers.toSet))

  def selfConnectionDetails: Set[ConnectionDescriptor] = selfDetails

  def repairTick(): Unit = broadcastIO.foreach(_.tick())

  def overlayInfoTick(): Unit = {
    broadcastIO.foreach { io =>
      given LocalUid = localUid
      publish(OverlayStatusProtocol.statusDelta(state, io, nowMillis()))
    }
  }

  def addOverlayConnection(latent: LatentConnection[Connection]): Unit =
    broadcastIO.foreach(_.addClientConnection(latent))

  def activeView: Set[Uid] = broadcastIO.collect {
    case io if io.overlayController.isInstanceOf[HyParViewStateMachine] =>
      io.overlayController.asInstanceOf[HyParViewStateMachine].activeView
  }.getOrElse(Set.empty)

  def passiveView: Set[Uid] = broadcastIO.collect {
    case io if io.overlayController.isInstanceOf[HyParViewStateMachine] =>
      io.overlayController.asInstanceOf[HyParViewStateMachine].passiveView
  }.getOrElse(Set.empty)

  def eagerView: Set[Uid] = broadcastIO.flatMap(io =>
    io.plumtreeState match
        case pt: PlumtreeBroadcast[?] => Some(pt.eagerPeers)
        case _                        => None
  ).getOrElse(Set.empty)

  def lastIncomingMessageTimes: Map[Uid, Long] = Map.empty

  def stop(): Unit = {
    abort.abort()
    timer.cancel()
  }
}
