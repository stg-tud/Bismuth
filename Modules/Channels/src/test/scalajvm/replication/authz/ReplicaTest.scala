package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromArray, writeToArray}
import crypto.Commitment.RevealedValue
import crypto.Hash
import crypto.channels.PrivateIdentity
import munit.FunSuite
import rdts.filters.PermissionTree
import replication.authz.ArdtEvent.Payload.{Capability, Revocation}
import replication.authz.AuthzTestSupport.{*, given}

import scala.collection.mutable

class ReplicaTest extends FunSuite {

  private def newReplicaWithGenesis(
      identity: PrivateIdentity,
      genesisEvent: ArdtEvent,
      onStateChange: Set[Int] => Unit = _ => ()
  ): (Replica[Set[Int]], MockAntiEntropy) = {
    var mockRef: MockAntiEntropy | Null = null
    val replica                         = new Replica[Set[Int]](
      genesisEvent.hash,
      identity,
      r => {
        val m = new MockAntiEntropy(r)
        mockRef = m
        m
      },
      onStateChange
    )
    replica.start()
    assert(replica.receiveEvent(writeToArray(genesisEvent)).isRight)
    (replica, mockRef.nn)
  }

  private def newReplica(
      onStateChange: Set[Int] => Unit = _ => ()
  ): (Replica[Set[Int]], MockAntiEntropy, PrivateIdentity, ArdtEvent) = {
    val rootIdentity    = newPrivateIdentity()
    val genesisEvent    = Authorization.createGenesis(rootIdentity)
    val (replica, mock) = newReplicaWithGenesis(rootIdentity, genesisEvent, onStateChange)
    (replica, mock, rootIdentity, genesisEvent)
  }

  // --- construction & basic queries ---

  test("localReplicaId matches the identity's public key") {
    val (replica, _, rootIdentity, _) = newReplica()
    assertEquals(replica.localReplicaId, rootIdentity.getPublic)
  }

  test("state is empty before any deltas are applied") {
    val (replica, _, _, _) = newReplica()
    assertEquals(replica.state, Set.empty[Int])
  }

  test("containsEvent is true for the genesis event and false for an unknown hash") {
    val (replica, _, _, genesisEvent) = newReplica()
    assert(replica.containsEvent(genesisEvent.hash))
    assert(!replica.containsEvent(Hash.compute("unknown".getBytes)))
  }

  // --- receiveEvent ---

  test("receiveEvent accepts a valid new event authorized by the genesis capability") {
    val (replica, _, rootIdentity, genesisEvent) = newReplica()
    val (deltaEvent, _)                          =
      buildDeltaEvent(
        Set(1),
        rootIdentity.getPublic,
        rootIdentity.identityKey.getPrivate,
        Set(genesisEvent.hash),
        genesisEvent.hash
      )

    assertEquals(replica.receiveEvent(writeToArray(deltaEvent)), Right(Some(deltaEvent.hash)))
    assert(replica.containsEvent(deltaEvent.hash))
  }

  test("receiveEvent returns Left with the missing parent when a parent is not locally available") {
    val (replica, _, rootIdentity, genesisEvent) = newReplica()
    val (missingParentEvent, _)                  =
      buildDeltaEvent(
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

    replica.receiveEvent(writeToArray(deltaEvent)) match {
      case Left(missing) => assert(missing.contains(missingParentEvent.hash))
      case Right(_)      => fail("expected Left because the parent event is not locally available")
    }
  }

  test("receiveEvent is idempotent, returning Right(None) when re-receiving an already-known event") {
    val (replica, _, _, genesisEvent) = newReplica()
    assertEquals(replica.receiveEvent(writeToArray(genesisEvent)), Right(None))
  }

  // --- receiveDelta ---

  test("receiveDelta stores a readable delta and notifies onStateChange with the merged state") {
    val notifications                            = mutable.Buffer.empty[Set[Int]]
    val (replica, _, rootIdentity, genesisEvent) = newReplica(onStateChange = s => notifications += s: Unit)
    val (deltaEvent, revealed)                   =
      buildDeltaEvent(
        Set(1, 2),
        rootIdentity.getPublic,
        rootIdentity.identityKey.getPrivate,
        Set(genesisEvent.hash),
        genesisEvent.hash
      )
    assert(replica.receiveEvent(writeToArray(deltaEvent)).isRight)

    replica.receiveDelta(deltaEvent.hash, revealed)

    assertEquals(replica.state, Set(1, 2))
    assertEquals(notifications.toList, List(Set(1, 2)))
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

    val (restrictedReplica, _) = newReplicaWithGenesis(restrictedIdentity, genesisEvent)
    assert(restrictedReplica.receiveEvent(writeToArray(delegation)).isRight)
    assert(restrictedReplica.receiveEvent(writeToArray(deltaEvent)).isRight)

    intercept[IllegalArgumentException] {
      restrictedReplica.receiveDelta(deltaEvent.hash, revealed)
    }
  }

  // --- mutateState (auto-selected capability) ---

  test("mutateState(mutator) uses the owned unrevoked capability, updates state, and broadcasts events and deltas") {
    val (replica, mock, _, _) = newReplica()

    replica.mutateState(_ => Set(42))

    assertEquals(replica.state, Set(42))
    assertEquals(mock.broadcastedEvents.size, 1)
    assertEquals(mock.broadcastedDeltas.size, 1)
    val broadcastDelta = mock.broadcastedDeltas.head
    assertEquals(readFromArray[Set[Int]](broadcastDelta.delta.value), Set(42))
  }

  test("mutateState(mutator) throws exception when no owned, unrevoked capability allows the delta") {
    val (replica, _, rootIdentity, genesisEvent) = newReplica()
    val revocation                               =
      buildEvent(
        Revocation(genesisEvent.hash),
        rootIdentity.getPublic,
        rootIdentity.identityKey.getPrivate,
        Set(genesisEvent.hash),
        genesisEvent.hash
      )
    assert(replica.receiveEvent(writeToArray(revocation)).isRight)

    intercept[IllegalArgumentException] {
      replica.mutateState(_ => Set(1))
    }
  }

  // --- mutateState (explicit capability) ---

  test("mutateState(mutator, capability) creates an update under the given capability") {
    val (replica, mock, _, genesisEvent) = newReplica()

    replica.mutateState(_ => Set(7), genesisEvent.hash)

    assertEquals(replica.state, Set(7))
    assertEquals(mock.broadcastedEvents.size, 1)
  }

  test("mutateState(mutator, capability) rejects writing under a revoked capability") {
    val (replica, _, rootIdentity, genesisEvent) = newReplica()
    val revocation                               =
      buildEvent(
        Revocation(genesisEvent.hash),
        rootIdentity.getPublic,
        rootIdentity.identityKey.getPrivate,
        Set(genesisEvent.hash),
        genesisEvent.hash
      )
    assert(replica.receiveEvent(writeToArray(revocation)).isRight)

    intercept[IllegalArgumentException] {
      replica.mutateState(_ => Set(1), genesisEvent.hash)
    }
  }

  test("mutateState(mutator, capability) rejects a delta the given capability's write permission forbids") {
    val (replica, _, rootIdentity, genesisEvent) = newReplica()
    val selfDelegation                           = buildEvent(
      Capability(rootIdentity.getPublic, PermissionTree.allow, PermissionTree.empty),
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(genesisEvent.hash),
      genesisEvent.hash
    )
    assert(replica.receiveEvent(writeToArray(selfDelegation)).isRight)

    intercept[IllegalArgumentException] {
      replica.mutateState(_ => Set(1), selfDelegation.hash)
    }
  }

  // --- createDelegation ---

  test("createDelegation creates a narrower delegation, applies it locally, and broadcasts it") {
    val (replica, mock, _, genesisEvent) = newReplica()
    val (delegateHolder, _)              = newIdentity()

    replica.createDelegation(genesisEvent.hash, delegateHolder, PermissionTree.fromPath("a"), PermissionTree.empty)

    assertEquals(mock.broadcastedEvents.size, 1)
    val delegationEvent = readFromArray[ArdtEvent](mock.broadcastedEvents.head)
    assertEquals(
      delegationEvent.payload,
      Capability(delegateHolder, PermissionTree.fromPath("a"), PermissionTree.empty): ArdtEvent.Payload
    )
    assert(replica.containsEvent(delegationEvent.hash))
  }

  test("createDelegation rejects permissions that exceed the used capability's permissions") {
    val (replica, _, rootIdentity, genesisEvent) = newReplica()
    val narrowSelfDelegation                     = buildEvent(
      Capability(rootIdentity.getPublic, PermissionTree.fromPath("a"), PermissionTree.empty),
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(genesisEvent.hash),
      genesisEvent.hash
    )
    assert(replica.receiveEvent(writeToArray(narrowSelfDelegation)).isRight)

    val (delegateHolder, _) = newIdentity()
    intercept[IllegalArgumentException] {
      replica.createDelegation(narrowSelfDelegation.hash, delegateHolder, PermissionTree.allow, PermissionTree.empty)
    }
  }

  test("createDelegation rejects writePermissions that are not <= readPermissions") {
    val (replica, _, _, genesisEvent) = newReplica()
    val (delegateHolder, _)           = newIdentity()

    intercept[IllegalArgumentException] {
      replica.createDelegation(genesisEvent.hash, delegateHolder, PermissionTree.empty, PermissionTree.allow)
    }
  }

  test("createDelegation throws IllegalArgumentException when usedCapability does not reference a Capability event") {
    val (replica, _, rootIdentity, genesisEvent) = newReplica()
    val (deltaEvent, _)                          =
      buildDeltaEvent(
        Set(1),
        rootIdentity.getPublic,
        rootIdentity.identityKey.getPrivate,
        Set(genesisEvent.hash),
        genesisEvent.hash
      )
    assert(replica.receiveEvent(writeToArray(deltaEvent)).isRight)

    val (delegateHolder, _) = newIdentity()
    intercept[IllegalArgumentException] {
      replica.createDelegation(deltaEvent.hash, delegateHolder, PermissionTree.empty, PermissionTree.empty)
    }
  }

  // --- createRevocation ---

  test("createRevocation revokes a capability owned by the local replica") {
    val (replica, mock, _, genesisEvent) = newReplica()
    replica.createRevocation(genesisEvent.hash)

    assertEquals(mock.broadcastedEvents.size, 1)
    val revocationEvent = readFromArray[ArdtEvent](mock.broadcastedEvents.head)
    assertEquals(revocationEvent.payload, Revocation(genesisEvent.hash): ArdtEvent.Payload)
  }

  test("createRevocation throws IllegalStateException when no owned capability is found in the authorization chain") {
    val rootIdentity          = newPrivateIdentity()
    val genesisEvent          = Authorization.createGenesis(rootIdentity)
    val bystanderIdentity     = newPrivateIdentity()
    val (bystanderReplica, _) = newReplicaWithGenesis(bystanderIdentity, genesisEvent)

    intercept[IllegalStateException] {
      bystanderReplica.createRevocation(genesisEvent.hash)
    }
  }

  // --- filterDeltas ---

  test("filterDeltas keeps only deltas the requesting replica may read") {
    val (replica, _, rootIdentity, genesisEvent) = newReplica()
    val (readableDelta, readableRevealed)        = buildDeltaEvent(
      Set.empty[Int],
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(genesisEvent.hash),
      genesisEvent.hash
    )
    assert(replica.receiveEvent(writeToArray(readableDelta)).isRight)
    replica.receiveDelta(readableDelta.hash, readableRevealed)

    val restrictedIdentity = newPrivateIdentity()
    val delegation         = buildEvent(
      Capability(restrictedIdentity.getPublic, PermissionTree.empty, PermissionTree.allow),
      rootIdentity.getPublic,
      rootIdentity.identityKey.getPrivate,
      Set(readableDelta.hash),
      genesisEvent.hash
    )
    assert(replica.receiveEvent(writeToArray(delegation)).isRight)
    val (unreadableDelta, unreadableRevealed) = buildDeltaEvent(
      Set(2),
      restrictedIdentity.getPublic,
      restrictedIdentity.identityKey.getPrivate,
      Set(delegation.hash),
      delegation.hash
    )
    assert(replica.receiveEvent(writeToArray(unreadableDelta)).isRight)
    replica.receiveDelta(unreadableDelta.hash, unreadableRevealed)

    val result = replica.filterDeltas(
      restrictedIdentity.getPublic,
      List(
        (eventHash = readableDelta.hash, delta = readableRevealed),
        (eventHash = unreadableDelta.hash, delta = unreadableRevealed)
      )
    )

    assertEquals(result.map(_.eventHash).toSet, Set(readableDelta.hash))
  }
}

class MockAntiEntropy(replica: Replica[?]) extends AntiEntropy(replica, _ => ???, _ => ???) {
  val broadcastedEvents: mutable.Buffer[Array[Byte]]                             = mutable.Buffer.empty
  val broadcastedDeltas: mutable.Buffer[(eventHash: Hash, delta: RevealedValue)] = mutable.Buffer.empty

  override def start(): Unit = ()

  override def listenAddress: Option[(String, Int)] = None

  override def broadcastEvents(events: Iterable[Array[Byte]]): Unit =
    broadcastedEvents ++= events: Unit

  override def broadcastDeltasFiltered(deltas: Iterable[(eventHash: Hash, delta: RevealedValue)]): Unit =
    broadcastedDeltas ++= deltas: Unit
}
