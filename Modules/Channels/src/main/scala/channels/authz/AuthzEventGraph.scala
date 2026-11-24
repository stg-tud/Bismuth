package channels.authz

import AuthzEvent.Payload.{Capability, Revocation}
import channels.crypto.{Hash, PublicIdentity, Verifier}
import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray

import scala.annotation.tailrec
import scala.util.Try

/** The validated event graph: an [[AuthzDag]] whose events are accepted by the [[CapabilityPolicy]], and the events
  * waiting for missing predecessors.
  */
case class AuthzEventGraph(
    dag: AuthzDag,
    policy: CapabilityPolicy,
    /** Events that are waiting for missing predecessors, by their hash. */
    waiting: Map[Hash, Array[Byte]] = Map.empty,
) {
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
      copy(dag = dag.added(eventHash, event), policy = policy.added(dag, eventHash, event))
    }
  }

  /** Contained or waiting for predecessors. */
  def received(eventHash: Hash): Boolean = events.contains(eventHash) || waiting.contains(eventHash)

  /** Events we know to exist (as predecessors of waiting events), but do not have. */
  def missing: Set[Hash] =
    waiting.values.flatMap(encoded => AuthzEventGraph.dependencies(readFromArray[AuthzEvent](encoded)))
      .filterNot(received).toSet

  /** Like [[receive]], but keeps the event until its missing predecessors arrive.
    * @return Right(Some(hash)) if the event was added, Right(None) if it was already known, and Left(missing) if it
    *         was kept because it depends on missing events
    * @throws IllegalArgumentException if the event is invalid
    */
  def receiveOrWait(encodedEvent: Array[Byte])(using Verifier): (AuthzEventGraph, Either[Set[Hash], Option[Hash]]) =
    receive(encodedEvent) match
        case Left(missingPredecessors) =>
          val hash = Hash.compute(encodedEvent)
          (copy(waiting = waiting.updated(hash, encodedEvent)), Left(missingPredecessors))
        case Right(graph) if graph eq this => (this, Right(None))
        case Right(graph)                  =>
          val hash = Hash.compute(encodedEvent)
          (graph.copy(waiting = waiting - hash), Right(Some(hash)))

  /** Like [[receiveOrWait]], but drops invalid events, as needed for events received from the network.
    * `onAdded` is called with the graph right after the event was added. If it throws, the event is dropped as well.
    * @return the next graph and state, and the hash of the event if it was added
    */
  def receiveOrDrop[S](encodedEvent: Array[Byte], state: S)(onAdded: (S, AuthzEventGraph, Hash) => S)(using
      Verifier
  ): (AuthzEventGraph, S, Option[Hash]) =
    Try {
      receiveOrWait(encodedEvent) match
          case (graph, Right(Some(hash))) => (graph, onAdded(state, graph, hash), Some(hash))
          case (graph, _)                 => (graph, state, None)
    }.getOrElse((copy(waiting = waiting - Hash.compute(encodedEvent)), state, None))

  /** Retries waiting events, see [[receiveOrDrop]], until no more can be added.
    * @return the next graph and state, and the events that were added, encoded, in the order they were added
    */
  def retryWaiting[S](state: S)(onAdded: (S, AuthzEventGraph, Hash) => S)(using
      Verifier
  ): (AuthzEventGraph, S, List[Array[Byte]]) = {
    @tailrec
    def loop(graph: AuthzEventGraph, state: S, added: List[Array[Byte]]): (AuthzEventGraph, S, List[Array[Byte]]) = {
      val (nextGraph, nextState, addedNow) = graph.waiting.values.foldLeft((graph, state, List.empty[Array[Byte]])) {
        case ((graph, state, acc), encoded) =>
          val (g, s, hash) = graph.receiveOrDrop(encoded, state)(onAdded)
          (g, s, if hash.nonEmpty then encoded :: acc else acc)
      }
      if addedNow.isEmpty then (nextGraph, nextState, added)
      else loop(nextGraph, nextState, added ++ addedNow.reverse)
    }
    loop(this, state, Nil)
  }
}

object AuthzEventGraph {

  /** The events that must be in the graph before `event` can be added. */
  def dependencies(event: AuthzEvent): Set[Hash] =
    val authorization = Option.when(event.authorization != Hash.allZeroHash)(event.authorization)
    event.payload match
        case Revocation(revoked) => event.parents ++ authorization + revoked
        case _                   => event.parents ++ authorization

  def apply(genesis: AuthzEvent): AuthzEventGraph = AuthzEventGraph(AuthzDag(genesis), CapabilityPolicy(genesis))

  def apply(genesis: Hash): AuthzEventGraph = AuthzEventGraph(AuthzDag(genesis), CapabilityPolicy.empty)
}
