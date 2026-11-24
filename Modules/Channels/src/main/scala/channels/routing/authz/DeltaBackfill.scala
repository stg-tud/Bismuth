package channels.routing.authz

import channels.authz.{AuthzReplica, DeltaValue}
import channels.connection.Peer
import channels.crypto.Hash
import channels.routing.Routing.Effect

/** The delta values of an [[channels.authz.AuthzReplica]], next to its events (which [[AuthzProtocol]] disseminates
  * with plumtree): which values to send to whom, filtered for what each receiver may read, and when to ask for missing
  * ones. Only decides, the replica and the peers are passed in.
  */
final class DeltaBackfill[RDT] {
  type Effects = List[Effect[AuthzMessage, Nothing]]

  /** A peer connected: ask it for the deltas we do not have. */
  def peerAdded(replica: AuthzReplica[RDT], peer: Peer): Effects = request(replica, List(peer))

  /** Events were pushed to peers: send the deltas of those events along, as far as each peer may read them. */
  def eventsPushed(replica: AuthzReplica[RDT], pushed: Iterable[(Peer, List[Hash])]): Effects =
    pushed.toList.flatMap((peer, hashes) => deltasFor(replica, hashes, peer))

  /** The replica changed from `before`: a new capability may make deltas readable that were withheld from us, ask
    * `peers` for them.
    */
  def replicaChanged(before: AuthzReplica[RDT], after: AuthzReplica[RDT], peers: Iterable[Peer]): Effects = {
    val capabilitiesBefore = before.eventGraph.capabilities(after.self).map(_._1)
    val gained             = after.eventGraph.capabilities(after.self).exists((hash, _) => !capabilitiesBefore(hash))
    if gained then request(after, peers.toList) else Nil
  }

  /** Deltas arrived from `from`: accept them, and forward the newly accepted ones to `peers` (except `from`), as the
    * events may have been forwarded before the deltas arrived.
    */
  def received(
      replica: AuthzReplica[RDT],
      from: Peer,
      peers: Iterable[Peer],
      values: List[DeltaValue]
  ): (AuthzReplica[RDT], Effects) = {
    val after    = replica.receiveDeltas(values)
    val accepted = values.map(_.event).filter(hash => replica.delta(hash).isEmpty && after.delta(hash).nonEmpty)
    (after, peers.iterator.filter(_ != from).toList.flatMap(deltasFor(after, accepted, _)))
  }

  /** `from` asked for the deltas of `hashes`: answer with those it may read. */
  def requested(replica: AuthzReplica[RDT], from: Peer, hashes: Set[Hash]): Effects =
    deltasFor(replica, hashes.toList, from)

  /** Ask `peers` for the deltas we do not have (they answer with those we may read). */
  private def request(replica: AuthzReplica[RDT], peers: List[Peer]): Effects =
    val wanted = replica.deltasWithoutValue
    if wanted.isEmpty || peers.isEmpty then Nil else List(Effect.Send(peers, AuthzMessage.RequestDeltas(wanted)))

  /** The stored deltas of `hashes` that `peer` may read, as a single message (if any). */
  private def deltasFor(replica: AuthzReplica[RDT], hashes: List[Hash], peer: Peer): Effects =
    AuthzProtocol.identityOf(peer).toList.flatMap { reader =>
      val readable = replica.readableDeltas(reader, hashes.filter(replica.containsEvent))
      Option.when(readable.nonEmpty)(Effect.Send(List(peer), AuthzMessage.Deltas(readable)))
    }
}
