package channels.broadcast.authz

import channels.MessagingIO
import channels.broadcast.Routing
import channels.broadcast.Routing.Timer
import channels.connection.{Peer, PeerConnectInfo}
import channels.crypto.{Hash, PrivateIdentity, PublicIdentity}
import channels.overlay.FullMeshOverlay
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.base.{Bottom, Decompose, Lattice, ReplicaId, Uid}
import rdts.filters.{Filter, PermissionTree}

/** [[Routing]] instance for [[AuthzReplica]]. Delivers the materialized state whenever it changes.
  * `identity` signs local events, see the extension methods in the companion.
  */
final class AuthzProtocol[RDT](val identity: PrivateIdentity)
    extends Routing {
  type State = AuthzReplica[RDT]
  type Msg   = AuthzMessage
  type Out   = RDT

  def addPeer(state: State, peer: Peer): state.Result                              = state.addPeer(peer)
  def removePeer(state: State, peer: Peer): state.Result                           = state.removePeer(peer)
  def handleMessage(state: State, from: Peer, message: AuthzMessage): state.Result = state.handleMessage(from, message)
  def timerFired(state: State, timer: Timer): state.Result                         = state.timerFired(timer)
}

object AuthzProtocol {

  /** A [[channels.MessagingIO]] for an authorization-aware replica of `RDT`, identified by `identity`.
    * Its overlay identifies itself as [[channels.connection.Peer.Authenticated]], so it only talks to peers over authenticated connections
    * (e.g., [[channels.connection.NioTCP.listenTls]]), whose identity is the one capabilities are granted to.
    */
  def messagingIO[RDT: {Lattice, Bottom, JsonValueCodec, Filter, Decompose}](
      identity: PrivateIdentity,
      genesis: Hash,
      onStateChange: RDT => Unit,
      config: MessagingIO.Config = MessagingIO.Config(),
  ): MessagingIO[AuthzProtocol[RDT]] = {
    val uid       = Uid(identity.getPublic.id)
    val withOwnId = config.copy(overlay =
      config.overlay.orElse(Some(FullMeshOverlay(PeerConnectInfo(Peer.Authenticated(uid), Set.empty))))
    )
    new MessagingIO[AuthzProtocol[RDT]](using AuthzProtocol[RDT](identity), AuthzMessage.codec)(
      ReplicaId.predefined(identity.getPublic.id),
      AuthzReplica.initial[RDT](identity.getPublic, genesis),
      onStateChange,
      withOwnId,
    )
  }

  /** Application API. Local operations throw if not permitted. */
  extension [RDT](io: MessagingIO[AuthzProtocol[RDT]]) {
    private def identity: PrivateIdentity = io.protocol.identity

    def replica: AuthzReplica[RDT] = io.state

    def receiveLocally(event: ArdtEvent): Unit = io.update(_.receiveLocally(event))

    def mutate(mutator: RDT => RDT): Unit = io.update(r => r.write(identity, mutator(r.state)))

    def mutate(mutator: RDT => RDT, capability: Hash): Unit =
      io.update(r => r.write(identity, mutator(r.state), capability))

    def delegate(usedCapability: Hash, delegatee: PublicIdentity, read: PermissionTree, write: PermissionTree): Unit =
      io.update(_.delegate(identity, usedCapability, delegatee, read, write))

    def revoke(revokedCapability: Hash): Unit = io.update(_.revoke(identity, revokedCapability))
  }
}
