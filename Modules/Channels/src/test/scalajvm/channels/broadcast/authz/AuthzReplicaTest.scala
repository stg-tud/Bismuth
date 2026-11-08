package channels.broadcast.authz

import channels.broadcast.BroadcastProtocol.{Effect, Step}
import channels.connection.Peer
import channels.crypto.PrivateIdentity
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import munit.FunSuite
import rdts.base.Uid
import rdts.filters.PermissionTree
import channels.broadcast.authz.ArdtEvent.Payload.{Capability, Revocation}
import channels.broadcast.authz.AuthzTestSupport.{*, given}
import channels.crypto.Hash
import channels.JsoniterCodecsJvm.ardtEventCodec

/** The local operations and direct inputs of [[AuthzReplica]]. See [[AuthzProtocolTest]] for the network side. */
class AuthzReplicaTest extends FunSuite {

  type Replica = AuthzReplica[Set[Int]]
  type Result  = Step[Replica, AuthzMessage, Set[Int]]

  private def newReplicaWithGenesis(identity: PrivateIdentity, genesisEvent: ArdtEvent): Replica =
    AuthzReplica.initial[Set[Int]](identity.getPublic, genesisEvent.hash).receiveLocally(genesisEvent).state

  private def newReplica(): (Replica, PrivateIdentity, ArdtEvent) = {
    val rootIdentity = newPrivateIdentity()
    val genesisEvent = Authorization.createGenesis(rootIdentity)
    (newReplicaWithGenesis(rootIdentity, genesisEvent), rootIdentity, genesisEvent)
  }

  /** A peer of `identity`, to observe what is disseminated to it. */
  private def peerOf(identity: PrivateIdentity): Peer = Peer.Authenticated(Uid(identity.getPublic.id))

  /** Connect `replica` to a peer of `identity`, discarding the initial synchronization. */
  private def withPeer(replica: Replica, identity: PrivateIdentity): Replica = replica.addPeer(peerOf(identity)).state

  private def receive(replica: Replica, event: ArdtEvent): Replica = {
    val (next, result) = replica.receiveEvent(writeToArray(event))
    assert(result.isRight, s"$event was not accepted: $result")
    next
  }

  private def sentEvents(step: Result): List[ArdtEvent] =
    step.events.toList.collect { case Effect.Send(_, AuthzMessage.Events(events)) => events }.flatten
      .map(readFromArray[ArdtEvent](_))

  private def sentDeltas(step: Result): List[AuthzMessage.DeltaValue] =
    step.events.toList.collect { case Effect.Send(_, AuthzMessage.Deltas(deltas)) => deltas }.flatten

  private def revocationOf(capability: ArdtEvent, identity: PrivateIdentity, parents: Set[Hash]): ArdtEvent =
    buildEvent(Revocation(capability.hash), identity.getPublic, identity.identityKey.getPrivate, parents, capability.hash)

  // --- construction & basic queries ---

  test("self matches the identity's public key") {
    val (replica, rootIdentity, _) = newReplica()
    assertEquals(replica.self, rootIdentity.getPublic)
  }

  test("state is empty before any deltas are applied") {
    val (replica, _, _) = newReplica()
    assertEquals(replica.state, Set.empty[Int])
  }

  test("containsEvent is true for the genesis event and false for an unknown hash") {
    val (replica, _, genesisEvent) = newReplica()
    assert(replica.containsEvent(genesisEvent.hash))
    assert(!replica.containsEvent(Hash.compute("unknown".getBytes)))
  }

  // --- receiveEvent ---

  test("receiveEvent accepts a valid new event authorized by the genesis capability") {
    val (replica, rootIdentity, genesisEvent) = newReplica()
    val (deltaEvent, _)                       = buildDeltaEvent(
      Set(1),
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(genesisEvent.hash),
      genesisEvent.hash
    )

    val (next, result) = replica.receiveEvent(writeToArray(deltaEvent))
    assertEquals(result, Right(Some(deltaEvent.hash)))
    assert(next.containsEvent(deltaEvent.hash))
  }

  test("receiveEvent returns Left with the missing parent when a parent is not locally available") {
    val (replica, rootIdentity, genesisEvent) = newReplica()
    val (missingParentEvent, _)               = buildDeltaEvent(
      Set(1),
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(genesisEvent.hash),
      genesisEvent.hash
    )
    val (deltaEvent, _) = buildDeltaEvent(
      Set(2),
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(missingParentEvent.hash),
      genesisEvent.hash
    )

    replica.receiveEvent(writeToArray(deltaEvent))._2 match {
      case Left(missing) => assert(missing.contains(missingParentEvent.hash))
      case Right(_)      => fail("expected Left because the parent event is not locally available")
    }
  }

  test("receiveEvent is idempotent, returning Right(None) when re-receiving an already-known event") {
    val (replica, _, genesisEvent) = newReplica()
    assertEquals(replica.receiveEvent(writeToArray(genesisEvent))._2, Right(None))
  }

  test("receiveEvent throws for an invalid event") {
    val (replica, rootIdentity, genesisEvent) = newReplica()
    val (deltaEvent, _)                       = buildDeltaEvent(
      Set(1),
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(genesisEvent.hash),
      genesisEvent.hash
    )
    val (otherIdentity, _) = newIdentity()
    intercept[IllegalArgumentException](replica.receiveEvent(writeToArray(deltaEvent.copy(author = otherIdentity))))
  }

  // --- receiveDelta ---

  test("receiveDelta stores a readable delta and merges it into the state") {
    val (replica, rootIdentity, genesisEvent) = newReplica()
    val (deltaEvent, revealed)                = buildDeltaEvent(
      Set(1, 2),
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(genesisEvent.hash),
      genesisEvent.hash
    )

    val next = receive(replica, deltaEvent).receiveDelta(deltaEvent.hash, revealed)

    assertEquals(next.state, Set(1, 2))
    assertEquals(next.delta(deltaEvent.hash), Some(Set(1, 2)))
  }

  test("a delta received from a peer is delivered to the application") {
    val (replica, rootIdentity, genesisEvent) = newReplica()
    val (deltaEvent, revealed)                = buildDeltaEvent(
      Set(1, 2),
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(genesisEvent.hash),
      genesisEvent.hash
    )

    val step = receive(replica, deltaEvent)
      .handleMessage(peerOf(rootIdentity), AuthzMessage.Deltas(List(AuthzMessage.DeltaValue(deltaEvent.hash, revealed))))

    assertEquals(step.events.toList, List(Effect.Deliver(Set(1, 2))))
  }

  test("receiveDelta throws when the local replica may not read the delta") {
    val rootIdentity       = newPrivateIdentity()
    val genesisEvent       = Authorization.createGenesis(rootIdentity)
    val restrictedIdentity = newPrivateIdentity()

    val delegation = buildEvent(
      Capability(restrictedIdentity.getPublic, PermissionTree.empty, PermissionTree.allow),
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(genesisEvent.hash),
      genesisEvent.hash
    )
    val (deltaEvent, revealed) = buildDeltaEvent(
      Set(1),
      restrictedIdentity.getPublic,
      restrictedIdentity.identityKey.getPrivate,
      Set(delegation.hash),
      delegation.hash
    )

    val restricted = receive(receive(newReplicaWithGenesis(restrictedIdentity, genesisEvent), delegation), deltaEvent)

    intercept[IllegalArgumentException](restricted.receiveDelta(deltaEvent.hash, revealed))
  }

  // --- write (auto-selected capability) ---

  test("write uses the owned unrevoked capability, updates the state, and disseminates events and deltas") {
    val (replica, rootIdentity, _) = newReplica()

    val step = withPeer(replica, rootIdentity).write(rootIdentity, Set(42))

    assertEquals(step.state.state, Set(42))
    assertEquals(sentEvents(step).size, 1)
    assertEquals(sentDeltas(step).map(d => readFromArray[Set[Int]](d.value.value)), List(Set(42)))
    assertEquals(step.events.last, Effect.Deliver(Set(42)))
  }

  test("write stores each decomposed part's own value, which another replica accepts when sent later") {
    val (replica, rootIdentity, genesisEvent) = newReplica()

    val step = withPeer(replica, rootIdentity).write(rootIdentity, Set(1, 2, 3))

    val events = sentEvents(step)
    assertEquals(events.size, 3)
    assertEquals(sentDeltas(step).size, 3)

    // values sent later, e.g. on request, are taken from the delta value store
    val other = events.foldLeft(newReplicaWithGenesis(rootIdentity, genesisEvent))(receive)
    val synced = events.foldLeft(other)((r, event) => r.receiveDelta(event.hash, step.state.deltaValue(event.hash).get))

    assertEquals(synced.state, Set(1, 2, 3))
  }

  test("write throws when no owned, unrevoked capability allows the delta") {
    val (replica, rootIdentity, genesisEvent) = newReplica()
    val revoked = receive(replica, revocationOf(genesisEvent, rootIdentity, Set(genesisEvent.hash)))

    intercept[IllegalArgumentException](revoked.write(rootIdentity, Set(1)))
  }

  // --- write (explicit capability) ---

  test("write with a capability creates an update under the given capability") {
    val (replica, rootIdentity, genesisEvent) = newReplica()

    val step = withPeer(replica, rootIdentity).write(rootIdentity, Set(7), genesisEvent.hash)

    assertEquals(step.state.state, Set(7))
    assertEquals(sentEvents(step).map(_.authorization), List(genesisEvent.hash))
  }

  test("write rejects writing under a revoked capability") {
    val (replica, rootIdentity, genesisEvent) = newReplica()
    val revoked = receive(replica, revocationOf(genesisEvent, rootIdentity, Set(genesisEvent.hash)))

    intercept[IllegalArgumentException](revoked.write(rootIdentity, Set(1), genesisEvent.hash))
  }

  test("write rejects a delta the given capability's write permission forbids") {
    val (replica, rootIdentity, genesisEvent) = newReplica()
    val selfDelegation                        = buildEvent(
      Capability(rootIdentity.getPublic, PermissionTree.allow, PermissionTree.empty),
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(genesisEvent.hash),
      genesisEvent.hash
    )

    intercept[IllegalArgumentException](
      receive(replica, selfDelegation).write(rootIdentity, Set(1), selfDelegation.hash)
    )
  }

  // --- delegate ---

  test("delegate creates a narrower delegation, applies it locally, and disseminates it") {
    val (replica, rootIdentity, genesisEvent) = newReplica()
    val (delegateHolder, _)                   = newIdentity()

    val step = withPeer(replica, rootIdentity)
      .delegate(rootIdentity, genesisEvent.hash, delegateHolder, PermissionTree.fromPath("a"), PermissionTree.empty)

    val List(delegationEvent) = sentEvents(step): @unchecked
    assertEquals(
      delegationEvent.payload,
      Capability(delegateHolder, PermissionTree.fromPath("a"), PermissionTree.empty): ArdtEvent.Payload
    )
    assert(step.state.containsEvent(delegationEvent.hash))
  }

  test("delegate rejects permissions that exceed the used capability's permissions") {
    val (replica, rootIdentity, genesisEvent) = newReplica()
    val narrowSelfDelegation                  = buildEvent(
      Capability(rootIdentity.getPublic, PermissionTree.fromPath("a"), PermissionTree.empty),
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(genesisEvent.hash),
      genesisEvent.hash
    )
    val (delegateHolder, _) = newIdentity()

    intercept[IllegalArgumentException](
      receive(replica, narrowSelfDelegation)
        .delegate(rootIdentity, narrowSelfDelegation.hash, delegateHolder, PermissionTree.allow, PermissionTree.empty)
    )
  }

  test("delegate rejects write permissions that are not <= read permissions") {
    val (replica, rootIdentity, genesisEvent) = newReplica()
    val (delegateHolder, _)                   = newIdentity()

    intercept[IllegalArgumentException](
      replica.delegate(rootIdentity, genesisEvent.hash, delegateHolder, PermissionTree.empty, PermissionTree.allow)
    )
  }

  test("delegate throws when the used capability does not reference a Capability event") {
    val (replica, rootIdentity, genesisEvent) = newReplica()
    val (deltaEvent, _)                       = buildDeltaEvent(
      Set(1),
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(genesisEvent.hash),
      genesisEvent.hash
    )
    val (delegateHolder, _) = newIdentity()

    intercept[IllegalArgumentException](
      receive(replica, deltaEvent)
        .delegate(rootIdentity, deltaEvent.hash, delegateHolder, PermissionTree.empty, PermissionTree.empty)
    )
  }

  // --- revoke ---

  test("revoke revokes a capability held by the local replica") {
    val (replica, rootIdentity, genesisEvent) = newReplica()

    val step = withPeer(replica, rootIdentity).revoke(rootIdentity, genesisEvent.hash)

    assertEquals(sentEvents(step).map(_.payload), List(Revocation(genesisEvent.hash): ArdtEvent.Payload))
  }

  test("revoke revokes a capability delegated by the local replica, using the capability it holds") {
    val (replica, rootIdentity, genesisEvent) = newReplica()
    val (delegateHolder, _)                   = newIdentity()
    val delegated = replica.delegate(rootIdentity, genesisEvent.hash, delegateHolder, PermissionTree.allow, PermissionTree.allow).state
    val (delegation, _) = delegated.activeCapabilitiesOf(delegateHolder).head

    val step = withPeer(delegated, rootIdentity).revoke(rootIdentity, delegation)

    assertEquals(sentEvents(step).map(_.authorization), List(genesisEvent.hash))
    assertEquals(step.state.activeCapabilitiesOf(delegateHolder), Set.empty)
  }

  test("revoke throws when no capability held locally is found in the authorization chain") {
    val rootIdentity      = newPrivateIdentity()
    val genesisEvent      = Authorization.createGenesis(rootIdentity)
    val bystanderIdentity = newPrivateIdentity()

    intercept[IllegalStateException](
      newReplicaWithGenesis(bystanderIdentity, genesisEvent).revoke(bystanderIdentity, genesisEvent.hash)
    )
  }

  // --- readableDeltas ---

  test("readableDeltas keeps only deltas the requesting replica may read") {
    val (replica, rootIdentity, genesisEvent) = newReplica()
    val (readableDelta, readableRevealed)     = buildDeltaEvent(
      Set.empty[Int],
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(genesisEvent.hash),
      genesisEvent.hash
    )
    val withReadable = receive(replica, readableDelta).receiveDelta(readableDelta.hash, readableRevealed)

    val restrictedIdentity = newPrivateIdentity()
    val delegation         = buildEvent(
      Capability(restrictedIdentity.getPublic, PermissionTree.empty, PermissionTree.allow),
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(readableDelta.hash),
      genesisEvent.hash
    )
    val (unreadableDelta, unreadableRevealed) = buildDeltaEvent(
      Set(2),
      restrictedIdentity.getPublic,
      restrictedIdentity.identityKey.getPrivate,
      Set(delegation.hash),
      delegation.hash
    )
    val withBoth = receive(receive(withReadable, delegation), unreadableDelta)
      .receiveDelta(unreadableDelta.hash, unreadableRevealed)

    val result = withBoth.readableDeltas(restrictedIdentity.getPublic, List(readableDelta.hash, unreadableDelta.hash))

    assertEquals(result.map(_.event), List(readableDelta.hash))
  }
}
