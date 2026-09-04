package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import crypto.{Commitment, Hash, Signature}
import munit.FunSuite
import rdts.filters.PermissionTree
import replication.authz.ArdtEvent.Payload.{Capability, Revocation}
import replication.authz.AuthzTestSupport.{*, given}

class AuthorizationTest extends FunSuite {
  // --- createGenesis ---

  test("createGenesis produces a validly self-signed genesis event") {
    val rootIdentity = newPrivateIdentity()
    val genesis      = Authorization.createGenesis(rootIdentity)

    assertEquals(genesis.author, rootIdentity.getPublic)
    assertEquals(
      genesis.payload,
      Capability(rootIdentity.getPublic, PermissionTree.allow, PermissionTree.allow): ArdtEvent.Payload
    )
    assertEquals(genesis.parents, Set.empty[Hash])
    assertEquals(genesis.authorization, Hash.allZeroHash)
    assert(
      genesis.signature.verify(
        rootIdentity.getPublicKey,
        writeToArray(genesis.copy(signature = Signature.allZeroSignature))
      )
    )
  }

  test("createGenesis's result is accepted by both ArdtEventGraph.apply and receive") {
    val rootIdentity = newPrivateIdentity()
    val genesis      = Authorization.createGenesis(rootIdentity)

    val graph = ArdtEventGraph[Set[Int]](genesis)
    assertEquals(graph.genesis, genesis.hash)
    assertEquals(graph.heads, Set(genesis.hash))
    assertEquals(
      graph.capabilityCache(rootIdentity.getPublic),
      Set[(
          Hash,
          Capability
      )]((genesis.hash, Capability(rootIdentity.getPublic, PermissionTree.allow, PermissionTree.allow)))
    )

    val empty    = ArdtEventGraph[Set[Int]](genesis.hash)
    val received = receiveOrFail(empty, genesis)
    assertEquals(received.heads, Set(genesis.hash))
  }

  // --- mayRead (delta overload) ---

  test(
    "mayRead (delta overload) is true when the replica holds an unrevoked capability whose read permission allows the delta"
  ) {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (deltaEvent, _) = buildDeltaEvent(Set(1, 2, 3), holder, holderKey, Set(genesis.hash), genesis.hash)
    val updated         = receiveOrFail(graph, deltaEvent)

    assert(Authorization.mayRead(holder, deltaEvent.hash, Set(1, 2, 3), updated))
  }

  test("mayRead (delta overload) is false when the capability's read permission disallows the delta") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, delegateKey)             = newIdentity()
    val delegation                          = buildEvent(
      Capability(delegate, PermissionTree.empty, PermissionTree.allow),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, delegation)

    val (deltaEvent, _) = buildDeltaEvent(Set(1), delegate, delegateKey, Set(delegation.hash), delegation.hash)
    val graph2          = receiveOrFail(graph1, deltaEvent)

    assert(!Authorization.mayRead(delegate, deltaEvent.hash, Set(1), graph2))
    assert(Authorization.mayRead(delegate, deltaEvent.hash, Set.empty[Int], graph2))
  }

  test("mayRead (delta overload) is false when the capability was revoked causally-before the delta event") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, delegateKey)             = newIdentity()
    val delegation                          = buildEvent(
      Capability(delegate, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, delegation)

    val revocation = buildEvent(Revocation(delegation.hash), holder, holderKey, Set(delegation.hash), genesis.hash)
    val graph2     = receiveOrFail(graph1, revocation)

    val (deltaEvent, _) = buildDeltaEvent(Set(1), delegate, delegateKey, Set(revocation.hash), delegation.hash)
    val graph3          = receiveOrFail(graph2, deltaEvent)

    assert(!Authorization.mayRead(delegate, deltaEvent.hash, Set(1), graph3))
  }

  test("mayRead (delta overload) is true when the capability's revocation is only concurrent with the delta event") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, delegateKey)             = newIdentity()
    val delegation                          = buildEvent(
      Capability(delegate, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, delegation)

    val (deltaEvent, _) = buildDeltaEvent(Set(1), delegate, delegateKey, Set(delegation.hash), delegation.hash)
    val graph2          = receiveOrFail(graph1, deltaEvent)

    val revocation = buildEvent(Revocation(delegation.hash), holder, holderKey, Set(delegation.hash), genesis.hash)
    val graph3     = receiveOrFail(graph2, revocation)

    assert(Authorization.mayRead(delegate, deltaEvent.hash, Set(1), graph3))
  }

  test("mayRead (delta overload) is true when the capability's revocation is causally-after the delta event") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, delegateKey)             = newIdentity()
    val delegation                          = buildEvent(
      Capability(delegate, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, delegation)

    val (deltaEvent, _) = buildDeltaEvent(Set(1), delegate, delegateKey, Set(delegation.hash), delegation.hash)
    val graph2          = receiveOrFail(graph1, deltaEvent)

    val revocation = buildEvent(Revocation(delegation.hash), holder, holderKey, Set(deltaEvent.hash), genesis.hash)
    val graph3     = receiveOrFail(graph2, revocation)

    assert(Authorization.mayRead(delegate, deltaEvent.hash, Set(1), graph3))
  }

  test("mayRead (delta overload) is true when the replica holds multiple capabilities and at least one satisfies") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, delegateKey)             = newIdentity()
    val restrictive                         = buildEvent(
      Capability(delegate, PermissionTree.empty, PermissionTree.allow),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, restrictive)

    val permissive = buildEvent(
      Capability(delegate, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      Set(restrictive.hash),
      genesis.hash
    )
    val graph2 = receiveOrFail(graph1, permissive)
    assertEquals(graph2.capabilityCache(delegate).size, 2)

    val (deltaEvent, _) = buildDeltaEvent(Set(1), delegate, delegateKey, Set(permissive.hash), permissive.hash)
    val graph3          = receiveOrFail(graph2, deltaEvent)

    assert(Authorization.mayRead(delegate, deltaEvent.hash, Set(1), graph3))
  }

  test("mayRead (delta overload) returns false for replica without capabilities") {
    val (graph, _, _, genesis) = freshGraph()
    val (stranger, _)          = newIdentity()

    assert(!Authorization.mayRead(stranger, genesis.hash, Set.empty[Int], graph))
  }

  // --- mayRead (RevealedValue overload) ---
  test("mayRead (RevealedValue overload) is false when the event doesn't match the commitment hash") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (deltaEvent, revealedValue) = buildDeltaEvent(Set(1, 2, 3), holder, holderKey, Set(genesis.hash), genesis.hash)
    val (altDeltaEvent, altRevealedValue) = buildDeltaEvent(Set(1, 2, 3), holder, holderKey, Set(genesis.hash), genesis.hash)
    val updated         = receiveOrFail(receiveOrFail(graph, deltaEvent), altDeltaEvent)

    assert(Authorization.mayRead(holder, deltaEvent.hash, revealedValue, updated))
    assert(Authorization.mayRead(holder, altDeltaEvent.hash, altRevealedValue, updated))
    assert(!Authorization.mayRead(holder, deltaEvent.hash, altRevealedValue, updated))
    assert(!Authorization.mayRead(holder, altDeltaEvent.hash, revealedValue, updated))
  }


  // --- mayRead (graph+store overload) ---

  test(
    "mayRead (graph+store overload) delegates to the delta overload for a real DeltaCommitment event backed by a matching RevealedValue"
  ) {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (deltaEvent, revealed) = buildDeltaEvent(Set(1, 2, 3), holder, holderKey, Set(genesis.hash), genesis.hash)
    val updated                = receiveOrFail(graph, deltaEvent)
    val store                  = new DeltaValueStore[Set[Int]]()
    store.put(revealed)

    assert(Authorization.mayRead(holder, deltaEvent.hash, updated, store))
  }

  test("mayRead (graph+store overload) is false for a hash not present in eventGraph.events") {
    val (graph, holder, _, genesis) = freshGraph()
    val store                       = new DeltaValueStore[Set[Int]]()

    assert(!Authorization.mayRead(holder, Hash.compute("unknown".getBytes), graph, store))
  }

  test("mayRead (graph+store overload) is false when the event at the hash is not a DeltaCommitment") {
    val (graph, holder, _, genesis) = freshGraph()
    val store                       = new DeltaValueStore[Set[Int]]()

    assert(!Authorization.mayRead(holder, genesis.hash, graph, store))
  }

  test(
    "mayRead (graph+store overload) throws NoSuchElementException when the commitment is absent from the DeltaValueStore"
  ) {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (deltaEvent, _) = buildDeltaEvent(Set(1), holder, holderKey, Set(genesis.hash), genesis.hash)
    val updated         = receiveOrFail(graph, deltaEvent)
    val store           = new DeltaValueStore[Set[Int]]() // revealed value never put

    intercept[NoSuchElementException] {
      Authorization.mayRead(holder, deltaEvent.hash, updated, store)
    }
  }

  // --- mayWrite ---

  test(
    "mayWrite is true for a valid revealed delta whose write permission allows it and whose authorizing capability is unrevoked"
  ) {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (deltaEvent, revealed) = buildDeltaEvent(Set(1, 2, 3), holder, holderKey, Set(genesis.hash), genesis.hash)
    val updated                = receiveOrFail(graph, deltaEvent)

    assert(Authorization.mayWrite(updated, deltaEvent.hash, revealed))
  }

  test("mayWrite is false when the revealed value's commitment does not match the event's DeltaCommitment") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (deltaEvent, _) = buildDeltaEvent(Set(1, 2, 3), holder, holderKey, Set(genesis.hash), genesis.hash)
    val updated         = receiveOrFail(graph, deltaEvent)

    val unrelated = Commitment.commit(writeToArray(Set(9)))
    assert(!Authorization.mayWrite(updated, deltaEvent.hash, unrelated))
  }

  test("mayWrite throws IllegalArgumentException when the event at deltaEventHash is not a DeltaCommitment") {
    val (graph, _, _, genesis) = freshGraph()
    val revealed               = Commitment.commit(writeToArray(Set(1)))

    intercept[IllegalArgumentException] {
      Authorization.mayWrite(graph, genesis.hash, revealed)
    }
  }

  test("mayWrite is false when the authorizing capability's write permission disallows the delta") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, delegateKey)             = newIdentity()
    val delegation                          = buildEvent(
      Capability(delegate, PermissionTree.allow, PermissionTree.empty),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, delegation)

    val (deltaEvent, revealed) = buildDeltaEvent(Set(1), delegate, delegateKey, Set(delegation.hash), delegation.hash)
    val graph2                 = receiveOrFail(graph1, deltaEvent)

    assert(!Authorization.mayWrite(graph2, deltaEvent.hash, revealed))
  }

  test("mayWrite is true when every revocation of the authorizing capability is causally-after the delta event") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, delegateKey)             = newIdentity()
    val delegation                          = buildEvent(
      Capability(delegate, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, delegation)

    val (deltaEvent, revealed) = buildDeltaEvent(Set(1), delegate, delegateKey, Set(delegation.hash), delegation.hash)
    val graph2                 = receiveOrFail(graph1, deltaEvent)

    val revocation = buildEvent(Revocation(delegation.hash), holder, holderKey, Set(deltaEvent.hash), genesis.hash)
    val graph3     = receiveOrFail(graph2, revocation)

    assert(Authorization.mayWrite(graph3, deltaEvent.hash, revealed))
  }

  test("mayWrite is false when a revocation of the authorizing capability is causally-before the delta event") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, delegateKey)             = newIdentity()
    val delegation                          = buildEvent(
      Capability(delegate, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, delegation)

    val revocation = buildEvent(Revocation(delegation.hash), holder, holderKey, Set(delegation.hash), genesis.hash)
    val graph2     = receiveOrFail(graph1, revocation)

    val (deltaEvent, revealed) = buildDeltaEvent(Set(1), delegate, delegateKey, Set(revocation.hash), delegation.hash)
    val graph3                 = receiveOrFail(graph2, deltaEvent)

    assert(!Authorization.mayWrite(graph3, deltaEvent.hash, revealed))
  }

  test("mayWrite throws NoSuchElementException when deltaEventHash is not present in the graph") {
    val (graph, _, _, genesis) = freshGraph()
    val revealed               = Commitment.commit(writeToArray(Set(1)))

    intercept[NoSuchElementException] {
      Authorization.mayWrite(graph, Hash.compute("unknown".getBytes), revealed)
    }
  }

  test("mayWrite is false when the authorization does not reference a Capability event") {
    val (initialGraph, holder, holderKey, genesis) = freshGraph()
    val (firstDelta, _) = buildDeltaEvent(Set(1), holder, holderKey, Set(genesis.hash), genesis.hash)
    val graph1          = receiveOrFail(initialGraph, firstDelta)

    // receive() would reject an event authorized by a non-Capability event outright, so this state
    // is only reachable by fabricating the graph directly rather than going through receive().
    val (secondDelta, secondRevealed) =
      buildDeltaEvent(Set(2), holder, holderKey, Set(firstDelta.hash), firstDelta.hash)
    val fabricatedGraph = graph1.copy[Set[Int]](
      heads = Set(secondDelta.hash),
      events = graph1.events + (secondDelta.hash -> (secondDelta, graph1.nextEventIndex)),
      nextEventIndex = graph1.nextEventIndex + 1
    )

    assert(!Authorization.mayWrite(fabricatedGraph, secondDelta.hash, secondRevealed))
  }

  // --- materialize ---

  test("materialize merges all accepted DeltaCommitment events whose revealed values are present in the store") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delta1, revealed1) = buildDeltaEvent(Set(1), holder, holderKey, Set(genesis.hash), genesis.hash)
    val graph1              = receiveOrFail(graph, delta1)
    val (delta2, revealed2) = buildDeltaEvent(Set(2, 3), holder, holderKey, Set(delta1.hash), genesis.hash)
    val graph2              = receiveOrFail(graph1, delta2)

    val store = new DeltaValueStore[Set[Int]]()
    store.put(revealed1)
    store.put(revealed2)

    assertEquals(Authorization.materialize(graph2, store), Set(1, 2, 3))
  }

  test("materialize skips DeltaCommitment events whose revealed value is absent from the store") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delta1, revealed1) = buildDeltaEvent(Set(1), holder, holderKey, Set(genesis.hash), genesis.hash)
    val graph1              = receiveOrFail(graph, delta1)
    val (delta2, _)         = buildDeltaEvent(Set(2, 3), holder, holderKey, Set(delta1.hash), genesis.hash)
    val graph2              = receiveOrFail(graph1, delta2)

    val store = new DeltaValueStore[Set[Int]]()
    store.put(revealed1) // delta2's revealed value is never put

    assertEquals(Authorization.materialize(graph2, store), Set(1))
  }

  test("materialize skips DeltaCommitment events whose write permission disallows the delta") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, delegateKey)             = newIdentity()
    val delegation                          = buildEvent(
      Capability(delegate, PermissionTree.allow, PermissionTree.empty),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, delegation)

    val (deltaEvent, revealed) = buildDeltaEvent(Set(1), delegate, delegateKey, Set(delegation.hash), delegation.hash)
    val graph2                 = receiveOrFail(graph1, deltaEvent)

    val store = new DeltaValueStore[Set[Int]]()
    store.put(revealed)

    assertEquals(Authorization.materialize(graph2, store), Set.empty[Int])
  }

  test("materialize skips DeltaCommitment events written after their authorizing capability was revoked") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, delegateKey)             = newIdentity()
    val delegation                          = buildEvent(
      Capability(delegate, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, delegation)

    val revocation = buildEvent(Revocation(delegation.hash), holder, holderKey, Set(delegation.hash), genesis.hash)
    val graph2     = receiveOrFail(graph1, revocation)

    val (deltaEvent, revealed) = buildDeltaEvent(Set(1), delegate, delegateKey, Set(revocation.hash), delegation.hash)
    val graph3                 = receiveOrFail(graph2, deltaEvent)

    val store = new DeltaValueStore[Set[Int]]()
    store.put(revealed)

    assertEquals(Authorization.materialize(graph3, store), Set.empty[Int])
  }

}
