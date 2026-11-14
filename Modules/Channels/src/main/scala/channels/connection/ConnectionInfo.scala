package channels.connection

import rdts.base.Uid

/** Describes a connection.
  *
  * @param local           how the local end of the connection is reachable, if known
  * @param remote          how the remote end of the connection is reachable, if known
  * @param authenticatedPeer the replica id of the remote side, if the transport authenticated it
  *                        (e.g., TLS with `NioTls`); `None` for unauthenticated transports
  * @param details         free-form transport specific information
  */
case class ConnectionInfo(
    local: Option[ConnectionDescriptor] = None,
    remote: Option[ConnectionDescriptor] = None,
    authenticatedPeer: Option[Uid] = None,
    details: Map[String, String] = Map.empty,
)
