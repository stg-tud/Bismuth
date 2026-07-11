package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import crypto.Hash
import rdts.base.Lattice
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment, Revocation}

import scala.collection.mutable

case class EventGraph[T: Lattice](genesis: Hash, heads: Set[Hash], events: Map[Hash, ArdtEvent]) {

  /** Adds an event to the event graph unless the event is invalid or causally-before events are missing from the graph.
    *
    * @param encodedEvent The serialized form of the event
    * @throws IllegalArgumentException If the event is invalid
    * @return If successful, this returns Right(updatedGraph). If parents are missing, then this returns
    */
  def receive(encodedEvent: Array[Byte]): Either[Set[Hash], EventGraph[T]] = {
    val event: ArdtEvent = readFromArray(encodedEvent)
    // Ensure that no invalid events are stored
    // Signature verification
    require(event.signature.verify(event.author.publicKey, encodedEvent))
    val hash = event.hash
    if events.contains(hash) then return Right(this)

    // All events need predecessors except the genesis event
    if hash == genesis then require(event.parents.isEmpty)
    else require(event.parents.nonEmpty)

    // Used capability is locally known (implies validity) and both holder and event author are the same
    val authorizingCapability = events.get(event.authorization) match {
      case None => // Return missing capability and heads
        val missingParents = event.parents.filter(events.contains)
        return Left(missingParents + event.authorization)
      case Some(ArdtEvent(cap @ Capability(capabilityHolder, _, _), eventAuthor, _, _, _)) =>
        // Used capability matches the event author
        require(capabilityHolder == eventAuthor)
        cap
      case _ => // Referenced capability is not a capability event
        throw java.lang.IllegalArgumentException(s"Event with invalid capability: $event")
    }

    // All parents are locally available
    val missingParents = event.parents.filter(events.contains)
    if missingParents.nonEmpty then return Left(missingParents)

    // Payload dependent validity checks
    event.payload match {
      case DeltaCommitment(_)         =>
      case Capability(_, read, write) =>
        // Delegation validity
        read <= authorizingCapability.read && write <= authorizingCapability.write
      case Revocation(revokedCapabilities) =>
        // revocation is authorized if authorizing capability is also part of the authorization chain of the revoked capability
        revokedCapabilities.forall(authorizationChain(_).contains(event.authorization))
    }

    // Event is valid
    Right(copy(heads = (heads -- event.parents) + hash, events = events + (hash -> event)))
  }

  // This is essentially BFS
  def causallyBefore(event1: Hash, event2: Hash): Boolean = {
    if event1 == event2 then return false
    // if !events.contains(event1) || !events.contains(event2) then return false

    val toSearch = {
      val parents = events(event2).parents
      if parents.contains(event1) then return true
      mutable.Queue.from(parents)
    }
    val searched = mutable.Set(event2) // we don't have cycles, thus event2 is never found

    while toSearch.nonEmpty do {
      val next    = toSearch.dequeue()
      val parents = events(event2).parents
      if parents.contains(event1) then return true
      toSearch.enqueueAll(events(next).parents.diff(searched))
      searched += next
    }

    false
  }
  def causallyAfter(event1: Hash, event2: Hash): Boolean = causallyBefore(event2, event1)
  def concurrent(event1: Hash, event2: Hash): Boolean    = ???

  def authorizationChain(capHash: Hash): Seq[Hash] = {
    if capHash == genesis then Seq(genesis)
    else capHash +: authorizationChain(events(capHash).authorization) // assumes that chain is in local event graph
  }

}

object Authorization {
  def materialize[T](eventGraph: EventGraph[T], deltaValueStore: DeltaValueStore[T]): T = ???

  def mayRead[T](events: EventGraph[T]): Boolean = ???
}
