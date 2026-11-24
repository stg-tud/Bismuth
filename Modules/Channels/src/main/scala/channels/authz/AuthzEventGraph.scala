package channels.authz

import AuthzEvent.Payload.Capability
import channels.crypto.{Hash, PublicIdentity, Verifier}

/** The validated event graph: an [[AuthzDag]] whose events are accepted by the [[CapabilityPolicy]]. */
case class AuthzEventGraph(dag: AuthzDag, policy: CapabilityPolicy) {
  export dag.{
    genesis,
    heads,
    events,
    nextEventIndex,
    latestCutBefore,
    notBefore,
    causallyBefore,
    causallyAfter,
    concurrent,
    causalOrder,
    allEventsInCausalOrder
  }
  export policy.{revocations, capabilities, activeCapabilities, activeCapabilitiesOf}

  private[authz] def revocationCache: Map[Hash, Set[Hash]]                          = policy.revocationCache
  private[authz] def capabilityCache: Map[PublicIdentity, Set[(Hash, Capability)]] = policy.capabilityCache

  def authorizationChain(capHash: Hash): Seq[Hash] = policy.authorizationChain(dag, capHash)

  def deltasInvalidatedBy(revocationEventHash: Hash): Iterable[Int] = policy.deltasInvalidatedBy(dag, revocationEventHash)

  /** Adds an event to the event graph unless the event is invalid or causally-before events are missing from the graph.
    *
    * @param encodedEvent The serialized form of the event
    * @throws IllegalArgumentException If the event is invalid
    * @return If successful, this returns Right(updatedGraph). If parents are missing, then this returns
    */
  def receive(encodedEvent: Array[Byte])(using verifier: Verifier): Either[Set[Hash], AuthzEventGraph] = {
    // Check for duplicates before checking signature
    val eventHash = Hash.compute(encodedEvent)
    if events.contains(eventHash) then return Right(this)

    // Ensure that no invalid events are stored
    val event = dag.verified(eventHash, encodedEvent)
    policy.check(dag, eventHash, event).map { _ =>
      AuthzEventGraph(dag.added(eventHash, event), policy.added(dag, eventHash, event))
    }
  }
}

object AuthzEventGraph {
  def apply(genesis: AuthzEvent): AuthzEventGraph = AuthzEventGraph(AuthzDag(genesis), CapabilityPolicy(genesis))

  def apply(genesis: Hash): AuthzEventGraph = AuthzEventGraph(AuthzDag(genesis), CapabilityPolicy.empty)
}
