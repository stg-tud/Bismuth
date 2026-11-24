package channels.authz

import CausalOrder.*
import channels.crypto.{Hash, Signature, Verifier}
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}

import scala.collection.mutable

/** The hash DAG of signed [[AuthzEvent]]s: which events exist, their order of arrival, and their causal order.
  * Knows nothing about what the events mean, see [[AuthzEventGraph]] for their validation.
  */
case class AuthzDag(
    genesis: Hash,
    heads: Set[Hash],
    events: Map[Hash, (AuthzEvent, Int)],
    nextEventIndex: Int,
    // By event index: the index of the latest cut in the causal past of the event, the event itself included. A cut is
    // an event that was the only head right after it was received, and thus has every event received before it in its
    // causal past. Receiving further events never changes these, since they don't change any event's causal past.
    private[authz] val latestCuts: Vector[Int]
) {

  /** Decodes an event and checks its signature and that only the genesis event has no parents.
    * @throws IllegalArgumentException If the event is invalid
    */
  def verified(eventHash: Hash, encodedEvent: Array[Byte])(using verifier: Verifier): AuthzEvent = {
    val event: AuthzEvent = readFromArray(encodedEvent)
    // Signature verification: (need to blank signature and re-encode for verification)
    require(verifier.verify(
      event.author,
      writeToArray(event.copy(signature = Signature.allZeroSignature)),
      event.signature
    ))

    // All events need predecessors except the genesis event
    if eventHash != genesis then {
      require(event.parents.nonEmpty)
    } else {
      require(event.parents.isEmpty)
      require(event.authorization == Hash.allZeroHash)
    }
    event
  }

  /** Adds an event whose parents are all in the DAG. */
  def added(eventHash: Hash, event: AuthzEvent): AuthzDag = {
    val concurrentHeads = heads -- event.parents
    copy(
      heads = concurrentHeads + eventHash,
      events = events + (eventHash -> (event, nextEventIndex)),
      nextEventIndex = nextEventIndex + 1,
      latestCuts = latestCuts :+ (
        if concurrentHeads.isEmpty then nextEventIndex
        else latestCutBefore(event.parents)
      ),
    )
  }

  /** The index of the latest cut in the causal past of any of `parents`, or -1 if there is none: every event with an
    * index up to it is causally before an event built on top of `parents`.
    */
  def latestCutBefore(parents: Set[Hash]): Int =
    parents.iterator.map(parent => latestCuts(events(parent)._2)).maxOption.getOrElse(-1)

  /** The events not in the causal past of the known part of `hashes`, in the order they were received.
    * Every event up to the latest cut before `hashes` is in their causal past, so only the later ones are searched.
    */
  def notBefore(hashes: Set[Hash]): List[Hash] = {
    val known      = hashes.filter(events.contains)
    val cutoff     = latestCutBefore(known)
    val causalPast = mutable.BitSet()
    val toVisit    = mutable.Stack.from(known)
    while toVisit.nonEmpty do {
      val (event, index) = events(toVisit.pop())
      if index > cutoff && !causalPast.contains(index) then
          causalPast += index
          toVisit.pushAll(event.parents)
    }
    events.iterator.collect { case (hash, (_, index)) if index > cutoff && !causalPast.contains(index) => index -> hash }
      .toList.sortBy(_._1).map(_._2)
  }

  /** Whether `event1` is causally before `event2`, searching the causal past of `event2` backwards.
    *
    * An event received after `event2` cannot be causally before it. If `event1` is at most as old as the latest cut in the
    * causal past of `event2`, or of any event on the way, it is before it as well. Only events received after `event1`
    * need to be searched.
    */
  def causallyBefore(event1: Hash, event2: Hash): Boolean = {
    if event1 == event2 then return false

    val (index1, ev2, index2) = (events.get(event1), events.get(event2)) match {
      case (Some((_, index1)), Some((ev2, index2))) => (index1, ev2, index2)
      case _                                        => return false
    }

    if index1 > index2 then return false
    if index1 <= latestCuts(index2) then return true

    val visited = mutable.BitSet(index2)
    val toVisit = mutable.Stack.from(ev2.parents)
    while toVisit.nonEmpty do {
      val next = toVisit.pop()
      if next == event1 then return true
      val (nextEv, nextIndex) = events(next)
      if nextIndex > index1 && !visited.contains(nextIndex) then {
        if index1 <= latestCuts(nextIndex) then return true
        visited += nextIndex
        toVisit.pushAll(nextEv.parents)
      }
    }

    false
  }

  def causallyAfter(event1: Hash, event2: Hash): Boolean = causallyBefore(event2, event1)

  def concurrent(event1: Hash, event2: Hash): Boolean =
    events.contains(event1) && events.contains(event2) &&
    !causallyAfter(event1, event2) && !causallyAfter(event2, event1)

  def causalOrder(event1: Hash, event2: Hash): CausalOrder =
    if !events.contains(event1) || !events.contains(event2) then UNKNOWN
    else if event1 == event2 then EQUAL
    else if causallyBefore(event1, event2) then BEFORE
    else if causallyBefore(event2, event1) then AFTER
    else CONCURRENT

  def allEventsInCausalOrder: Array[(Hash, AuthzEvent)] = {
    val sortedEvents = Array.ofDim[(Hash, AuthzEvent)](events.size)
    events.foreach { case (hash, (event, i)) => sortedEvents(i) = hash -> event }
    sortedEvents
  }
}

object AuthzDag {

  /** A DAG containing only `genesis`. */
  def apply(genesis: AuthzEvent): AuthzDag = {
    val hash = genesis.hash
    AuthzDag(hash, Set(hash), Map(hash -> (genesis, 0)), 1, Vector(0))
  }

  /** An empty DAG that only knows the hash of the genesis event. */
  def apply(genesis: Hash): AuthzDag = AuthzDag(genesis, Set.empty, Map.empty, 0, Vector.empty)
}
