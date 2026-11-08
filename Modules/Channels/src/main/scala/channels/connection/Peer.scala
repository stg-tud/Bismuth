package channels.connection

import rdts.base.Uid

/** Identity of a remote node as seen by the overlay and broadcast layers.
  *
  * `Authenticated` and `Claimed` peers are distinct values, even with equal `uid`s: an authenticated peer can only
  * ever be bound to a connection whose transport authenticated exactly that `uid`, while a claimed peer can only be
  * bound to an unauthenticated connection. Thus, unauthenticated connections can never impersonate authenticated
  * peers, and messages for authenticated peers only travel over connections authenticated as that peer.
  */
enum Peer {

  /** Identity proven by the transport (e.g., the key of a TLS certificate), see [[ConnectionInfo.authenticatedPeer]]. */
  case Authenticated(uid: Uid)

  /** Identity only asserted by the remote side. */
  case Claimed(uid: Uid)

  def uid: Uid

  def isAuthenticated: Boolean = this match
      case Authenticated(_) => true
      case Claimed(_)       => false

  override def toString: String = this match
      case Authenticated(uid) => s"auth:${uid.toString}"
      case Claimed(uid)       => s"claim:${uid.toString}"
}

object Peer {

  /** Whether `peer` may be bound to `conn`: authenticated peers only to connections authenticated as them,
    * claimed peers only to unauthenticated connections.
    */
  def mayUse(conn: Connection, peer: Peer): Boolean =
    conn.info.authenticatedPeer match
        case Some(authenticated) => peer == Authenticated(authenticated)
        case None                => !peer.isAuthenticated

  given Ordering[Peer] = Ordering.by(peer => (peer.isAuthenticated, Uid.unwrap(peer.uid)))
}
