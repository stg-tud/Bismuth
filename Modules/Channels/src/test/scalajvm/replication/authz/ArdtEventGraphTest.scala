package replication.authz

import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import crypto.Hash
import munit.FunSuite
import rdts.filters.PermissionTree
import replication.authz.ArdtEvent.Payload.{Capability, DeltaCommitment, Revocation}
import replication.authz.AuthzTestSupport.*
import replication.authz.CausalOrder.*

class ArdtEventGraphTest extends FunSuite {

  // --- construction ---

  test("apply(genesisEvent) seeds genesis, heads, events and capabilityCache") {
    val (graph, holder, _, genesis) = freshGraph()
    assertEquals(graph.genesis, genesis.hash)
    assertEquals(graph.heads, Set(genesis.hash))
    assertEquals(graph.events(genesis.hash), (genesis, 0))
    assertEquals(graph.nextEventIndex, 1)
    assertEquals(
      graph.capabilityCache(holder),
      Set[(Hash, Capability)]((genesis.hash, Capability(holder, PermissionTree.allow, PermissionTree.allow)))
    )
    assertEquals(graph.revocationCache, Map.empty[Hash, Set[Hash]])
  }

  test("apply(genesisHash) creates an empty graph awaiting the given genesis hash") {
    val (_, _, _, genesis) = freshGraph()
    val empty              = ArdtEventGraph[Set[Int]](genesis.hash)
    assertEquals(empty.genesis, genesis.hash)
    assertEquals(empty.heads, Set.empty[Hash])
    assertEquals(empty.events, Map.empty[Hash, (ArdtEvent, Int)])
    assertEquals(empty.nextEventIndex, 0)

    empty.receive(writeToArray(genesis)) match {
      case Left(missing) => fail("empty graph should accept genesis")
      case Right(graph)  =>
        assertEquals(graph.genesis, genesis.hash)
        assertEquals(graph.heads, Set(genesis.hash))
        assertEquals(graph.events, Map(genesis.hash -> (genesis, 0)))
        assertEquals(graph.nextEventIndex, 1)
    }
  }

  // --- receive: duplicates & signatures ---

  test("receive returns Right(this) unchanged for an already-known event") {
    val (graph, _, _, genesis) = freshGraph()
    assertEquals(graph.receive(writeToArray(genesis)), Right(graph))
  }

  test("receive rejects an event whose signature does not match its content") {
    val (graph, _, _, genesis) = freshGraph()
    val (otherHolder, _)       = newIdentity()
    val tampered = genesis.copy(payload = Capability(otherHolder, PermissionTree.allow, PermissionTree.allow))
    intercept[IllegalArgumentException] {
      graph.receive(writeToArray(tampered))
    }
  }

  // --- receive: genesis-specific validity rules ---

  test("receive rejects a genesis event with non-empty parents") {
    val (holder, holderKey) = newIdentity()
    val badGenesis          = buildEvent(
      Capability(holder, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      parents = Set(Hash.allZeroHash),
      Hash.allZeroHash
    )
    val emptyGraph = ArdtEventGraph[Set[Int]](badGenesis.hash)
    intercept[IllegalArgumentException] {
      emptyGraph.receive(writeToArray(badGenesis))
    }
  }

  test("receive rejects a genesis event whose authorization is not the all-zero hash") {
    val (holder, holderKey) = newIdentity()
    val badGenesis          = buildEvent(
      Capability(holder, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      Set.empty,
      Hash.compute("not-zero".getBytes)
    )
    val emptyGraph = ArdtEventGraph[Set[Int]](badGenesis.hash)
    intercept[IllegalArgumentException] {
      emptyGraph.receive(writeToArray(badGenesis))
    }
  }

  test("receive rejects a genesis event whose payload is not a Capability") {
    val (holder, holderKey) = newIdentity()
    val badGenesis          = buildEvent(
      DeltaCommitment(Hash.allZeroHash),
      holder,
      holderKey,
      Set.empty,
      Hash.allZeroHash
    )
    val emptyGraph = ArdtEventGraph[Set[Int]](badGenesis.hash)
    intercept[IllegalArgumentException] {
      emptyGraph.receive(writeToArray(badGenesis))
    }
  }

  // --- receive: non-genesis structural rules ---

  test("receive rejects a non-genesis event with empty parents") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val noParents                           =
      buildEvent(DeltaCommitment(Hash.compute("delta".getBytes)), holder, holderKey, Set.empty, genesis.hash)
    intercept[IllegalArgumentException] {
      graph.receive(writeToArray(noParents))
    }
  }

  test("receive returns Left containing the authorization hash when it references an unknown capability") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val unknownAuthorization                = Hash.compute("unknown-capability".getBytes)
    val event                               =
      buildEvent(
        DeltaCommitment(Hash.compute("delta".getBytes)),
        holder,
        holderKey,
        Set(genesis.hash),
        unknownAuthorization
      )
    graph.receive(writeToArray(event)) match {
      case Left(missing) => assert(missing.contains(unknownAuthorization))
      case Right(_)      => fail("expected Left because the authorization hash is unknown")
    }
  }

  test("receive rejects an event authorized by a capability that was not granted to its author") {
    val (graph, _, _, genesis)  = freshGraph()
    val (impostor, impostorKey) = newIdentity()
    val event                   =
      buildEvent(
        DeltaCommitment(Hash.compute("delta".getBytes)),
        impostor,
        impostorKey,
        Set(genesis.hash),
        genesis.hash
      )
    intercept[IllegalArgumentException] {
      graph.receive(writeToArray(event))
    }
  }

  test("receive rejects an event whose authorization references a non-capability event") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val delta                               =
      buildEvent(DeltaCommitment(Hash.compute("delta".getBytes)), holder, holderKey, Set(genesis.hash), genesis.hash)
    val graph1 = receiveOrFail(graph, delta)

    val invalidlyAuthorized =
      buildEvent(DeltaCommitment(Hash.compute("delta2".getBytes)), holder, holderKey, Set(delta.hash), delta.hash)
    intercept[IllegalArgumentException] {
      graph1.receive(writeToArray(invalidlyAuthorized))
    }
  }

  test("receive returns Left when a parent event is not locally available") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val delta1                              =
      buildEvent(DeltaCommitment(Hash.compute("delta1".getBytes)), holder, holderKey, Set(genesis.hash), genesis.hash)
    val delta2 =
      buildEvent(DeltaCommitment(Hash.compute("delta2".getBytes)), holder, holderKey, Set(delta1.hash), genesis.hash)

    // graph only knows genesis, not delta1, so delta2 cannot be accepted yet
    graph.receive(writeToArray(delta2)) match {
      case Left(missing) => assert(missing.contains(delta1.hash))
      case Right(_)      => fail("expected Left because delta1 is not locally available")
    }
  }

  test("receive returns Left with exactly the unknown parents when some parents are known and some are not") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val known                               =
      buildEvent(DeltaCommitment(Hash.compute("known".getBytes)), holder, holderKey, Set(genesis.hash), genesis.hash)
    val graph1 = receiveOrFail(graph, known)

    val unknownParent = Hash.compute("unknown-parent".getBytes)
    val event         = buildEvent(
      DeltaCommitment(Hash.compute("delta".getBytes)),
      holder,
      holderKey,
      Set(known.hash, unknownParent),
      genesis.hash
    )

    graph1.receive(writeToArray(event)) match {
      case Left(missing) => assertEquals(missing, Set(unknownParent))
      case Right(_)      => fail("expected Left containing only the unknown parent")
    }
  }

  // --- receive: successful chaining ---

  test("receive accepts a DeltaCommitment event authorized by an already-known capability") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val delta                               =
      buildEvent(DeltaCommitment(Hash.compute("delta".getBytes)), holder, holderKey, Set(genesis.hash), genesis.hash)
    val updated = receiveOrFail(graph, delta)

    assertEquals(updated.heads, Set(delta.hash))
    assertEquals(updated.events(delta.hash), (delta, 1))
    assertEquals(updated.nextEventIndex, 2)
    assertEquals(updated.capabilityCache, graph.capabilityCache)
    assertEquals(updated.revocationCache, graph.revocationCache)
  }

  test("receive accepts a Capability delegation whose permissions narrow the authorizing capability") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, _)                       = newIdentity()
    val delegation                          = buildEvent(
      Capability(delegate, PermissionTree.fromPath("a"), PermissionTree.empty),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val updated = receiveOrFail(graph, delegation)

    assertEquals(updated.heads, Set(delegation.hash))
    assertEquals(
      updated.capabilityCache(delegate),
      Set[(
          Hash,
          Capability
      )]((delegation.hash, Capability(delegate, PermissionTree.fromPath("a"), PermissionTree.empty)))
    )
  }

  test("receive accepts a Capability delegation whose permissions exactly equal the authorizing capability") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, delegateKey)             = newIdentity()
    val delegation                          = buildEvent(
      Capability(delegate, PermissionTree.fromPath("a"), PermissionTree.empty),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, delegation)

    val (subDelegate, _)   = newIdentity()
    val equalSubDelegation = buildEvent(
      Capability(subDelegate, PermissionTree.fromPath("a"), PermissionTree.empty),
      delegate,
      delegateKey,
      Set(delegation.hash),
      delegation.hash
    )
    val updated = receiveOrFail(graph1, equalSubDelegation)

    assertEquals(updated.heads, Set(equalSubDelegation.hash))
  }

  test("receive rejects a Capability delegation that escalates permissions beyond the authorizing capability") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, delegateKey)             = newIdentity()
    val narrowDelegation                    = buildEvent(
      Capability(delegate, PermissionTree.fromPath("a"), PermissionTree.empty),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, narrowDelegation)

    val (subDelegate, _) = newIdentity()
    val escalation       = buildEvent(
      Capability(subDelegate, PermissionTree.allow, PermissionTree.empty),
      delegate,
      delegateKey,
      Set(narrowDelegation.hash),
      narrowDelegation.hash
    )
    intercept[IllegalArgumentException] {
      graph1.receive(writeToArray(escalation))
    }
  }

  test("receive accepts a Revocation authorized by an ancestor in the revoked capability's chain") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, _)                       = newIdentity()
    val delegation                          = buildEvent(
      Capability(delegate, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, delegation)

    val revocation = buildEvent(
      Revocation(delegation.hash),
      holder,
      holderKey,
      Set(delegation.hash),
      genesis.hash
    )
    val graph2 = receiveOrFail(graph1, revocation)

    assertEquals(graph2.revocationCache(delegation.hash), Set(revocation.hash))
    assertEquals(graph2.revocations(delegation.hash), Set(revocation.hash))
  }

  test("receive rejects a Revocation whose authorizing capability is not part of the revoked capability's chain") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (branchA, _)                        = newIdentity()
    val (branchB, branchBKey)               = newIdentity()

    val delegationA = buildEvent(
      Capability(branchA, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, delegationA)

    val delegationB = buildEvent(
      Capability(branchB, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      Set(delegationA.hash),
      genesis.hash
    )
    val graph2 = receiveOrFail(graph1, delegationB)

    // branchB tries to revoke the sibling branchA capability using its own (unrelated) capability
    val illegalRevocation = buildEvent(
      Revocation(delegationA.hash),
      branchB,
      branchBKey,
      Set(delegationB.hash),
      delegationB.hash
    )
    intercept[IllegalArgumentException] {
      graph2.receive(writeToArray(illegalRevocation))
    }
  }

  test("receive accepts a capability revoking itself") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val selfRevocation = buildEvent(Revocation(genesis.hash), holder, holderKey, Set(genesis.hash), genesis.hash)
    val updated        = receiveOrFail(graph, selfRevocation)

    assertEquals(updated.revocationCache(genesis.hash), Set(selfRevocation.hash))
    assertEquals(updated.revocations(genesis.hash), Set(selfRevocation.hash))
  }

  test("receive accepts a Revocation authorized by a distant ancestor, not just the immediate parent") {
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

    val (subDelegate, _) = newIdentity()
    val subDelegation    = buildEvent(
      Capability(subDelegate, PermissionTree.allow, PermissionTree.allow),
      delegate,
      delegateKey,
      Set(delegation.hash),
      delegation.hash
    )
    val graph2 = receiveOrFail(graph1, subDelegation)

    // genesis holder, two hops up the chain, revokes subDelegation directly without going through delegate
    val distantRevocation =
      buildEvent(Revocation(subDelegation.hash), holder, holderKey, Set(subDelegation.hash), genesis.hash)
    val updated = receiveOrFail(graph2, distantRevocation)

    assertEquals(updated.revocations(subDelegation.hash), Set(distantRevocation.hash))
  }

  test("revocationCache accumulates multiple independent revocations of the same capability") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val (delegate, _)                       = newIdentity()
    val delegation                          = buildEvent(
      Capability(delegate, PermissionTree.allow, PermissionTree.allow),
      holder,
      holderKey,
      Set(genesis.hash),
      genesis.hash
    )
    val graph1 = receiveOrFail(graph, delegation)

    val revocation1 = buildEvent(Revocation(delegation.hash), holder, holderKey, Set(delegation.hash), genesis.hash)
    val graph2      = receiveOrFail(graph1, revocation1)

    val revocation2 = buildEvent(Revocation(delegation.hash), holder, holderKey, Set(revocation1.hash), genesis.hash)
    val graph3      = receiveOrFail(graph2, revocation2)

    assertEquals(graph3.revocationCache(delegation.hash), Set(revocation1.hash, revocation2.hash))
    assertEquals(graph3.revocations(delegation.hash), Set(revocation1.hash, revocation2.hash))
  }

  // --- authorizationChain ---

  test("authorizationChain(genesis) is just the genesis hash") {
    val (graph, _, _, genesis) = freshGraph()
    assertEquals(graph.authorizationChain(genesis.hash), Seq(genesis.hash))
  }

  test("authorizationChain follows authorization links back to genesis") {
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

    val (subDelegate, _) = newIdentity()
    val subDelegation    = buildEvent(
      Capability(subDelegate, PermissionTree.allow, PermissionTree.allow),
      delegate,
      delegateKey,
      Set(delegation.hash),
      delegation.hash
    )
    val graph2 = receiveOrFail(graph1, subDelegation)

    assertEquals(graph2.authorizationChain(subDelegation.hash), Seq(subDelegation.hash, delegation.hash, genesis.hash))
  }

  // --- revocations ---

  test("revocations is empty for a capability that has not been revoked") {
    val (graph, _, _, genesis) = freshGraph()
    assertEquals(graph.revocations(genesis.hash), Set.empty[Hash])
  }

  test("revocations on a descendant include revocations recorded higher up its authorization chain") {
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

    val (subDelegate, _) = newIdentity()
    val subDelegation    = buildEvent(
      Capability(subDelegate, PermissionTree.allow, PermissionTree.allow),
      delegate,
      delegateKey,
      Set(delegation.hash),
      delegation.hash
    )
    val graph2 = receiveOrFail(graph1, subDelegation)

    val revokeDelegation =
      buildEvent(Revocation(delegation.hash), holder, holderKey, Set(subDelegation.hash), genesis.hash)
    val graph3 = receiveOrFail(graph2, revokeDelegation)

    assertEquals(graph3.revocations(subDelegation.hash), Set(revokeDelegation.hash))
    assertEquals(graph3.revocations(delegation.hash), Set(revokeDelegation.hash))
    assertEquals(graph3.revocations(genesis.hash), Set.empty[Hash])
  }

  // --- causal ordering ---

  private def siblingGraph(): (ArdtEventGraph[Set[Int]], ArdtEvent, ArdtEvent, ArdtEvent) = {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val childA                              =
      buildEvent(DeltaCommitment(Hash.compute("a".getBytes)), holder, holderKey, Set(genesis.hash), genesis.hash)
    val childB =
      buildEvent(DeltaCommitment(Hash.compute("b".getBytes)), holder, holderKey, Set(genesis.hash), genesis.hash)
    val graph1 = receiveOrFail(graph, childA)
    val graph2 = receiveOrFail(graph1, childB)
    (graph2, genesis, childA, childB)
  }

  test("causallyBefore/causallyAfter hold between a direct parent and its child") {
    val (graph, genesis, childA, _) = siblingGraph()
    assert(graph.causallyBefore(genesis.hash, childA.hash))
    assert(graph.causallyAfter(childA.hash, genesis.hash))
    assert(!graph.causallyBefore(childA.hash, genesis.hash))
    assert(!graph.causallyAfter(genesis.hash, childA.hash))
  }

  test("concurrent holds between sibling events and not between causally related ones") {
    val (graph, genesis, childA, childB) = siblingGraph()
    assert(graph.concurrent(childA.hash, childB.hash))
    assert(graph.concurrent(childB.hash, childA.hash))
    assert(!graph.concurrent(genesis.hash, childA.hash))
  }

  test("causalOrder reports EQUAL, BEFORE, AFTER, CONCURRENT and UNKNOWN correctly") {
    val (graph, genesis, childA, childB) = siblingGraph()
    assertEquals(graph.causalOrder(childA.hash, childA.hash), EQUAL)
    assertEquals(graph.causalOrder(genesis.hash, childA.hash), BEFORE)
    assertEquals(graph.causalOrder(childA.hash, genesis.hash), AFTER)
    assertEquals(graph.causalOrder(childA.hash, childB.hash), CONCURRENT)
    assertEquals(graph.causalOrder(childA.hash, Hash.compute("unknown".getBytes)), UNKNOWN)
  }

  test("causallyBefore is transitive across multiple hops") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val childA                              =
      buildEvent(DeltaCommitment(Hash.compute("a".getBytes)), holder, holderKey, Set(genesis.hash), genesis.hash)
    val graph1 = receiveOrFail(graph, childA)

    val grandchild =
      buildEvent(
        DeltaCommitment(Hash.compute("grandchild".getBytes)),
        holder,
        holderKey,
        Set(childA.hash),
        genesis.hash
      )
    val graph2 = receiveOrFail(graph1, grandchild)

    assert(graph2.causallyBefore(genesis.hash, grandchild.hash))
    assert(graph2.causallyAfter(grandchild.hash, genesis.hash))
    assertEquals(graph2.causalOrder(genesis.hash, grandchild.hash), BEFORE)
  }

  test("causallyBefore holds transitively across three or more hops") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val e1                                  =
      buildEvent(DeltaCommitment(Hash.compute("e1".getBytes)), holder, holderKey, Set(genesis.hash), genesis.hash)
    val graph1 = receiveOrFail(graph, e1)
    val e2     = buildEvent(DeltaCommitment(Hash.compute("e2".getBytes)), holder, holderKey, Set(e1.hash), genesis.hash)
    val graph2 = receiveOrFail(graph1, e2)
    val e3     = buildEvent(DeltaCommitment(Hash.compute("e3".getBytes)), holder, holderKey, Set(e2.hash), genesis.hash)
    val graph3 = receiveOrFail(graph2, e3)

    assert(graph3.causallyBefore(genesis.hash, e3.hash))
    assert(graph3.causallyBefore(e1.hash, e3.hash))
    assert(graph3.causallyBefore(e2.hash, e3.hash))
    assert(!graph3.causallyBefore(e3.hash, genesis.hash))
    assert(!graph3.causallyBefore(e3.hash, e1.hash))
    assertEquals(graph3.causalOrder(genesis.hash, e3.hash), BEFORE)
  }

  test("causallyBefore holds transitively through both branches of a diamond merge, whose siblings stay concurrent") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val branchB                             =
      buildEvent(DeltaCommitment(Hash.compute("b".getBytes)), holder, holderKey, Set(genesis.hash), genesis.hash)
    val graph1  = receiveOrFail(graph, branchB)
    val branchC =
      buildEvent(DeltaCommitment(Hash.compute("c".getBytes)), holder, holderKey, Set(genesis.hash), genesis.hash)
    val graph2 = receiveOrFail(graph1, branchC)
    val merge  = buildEvent(
      DeltaCommitment(Hash.compute("merge".getBytes)),
      holder,
      holderKey,
      Set(branchB.hash, branchC.hash),
      genesis.hash
    )
    val graph3 = receiveOrFail(graph2, merge)

    assert(graph3.causallyBefore(genesis.hash, merge.hash))
    assert(graph3.causallyBefore(branchB.hash, merge.hash))
    assert(graph3.causallyBefore(branchC.hash, merge.hash))
    assert(graph3.concurrent(branchB.hash, branchC.hash))
    assertEquals(graph3.heads, Set(merge.hash))
  }

  test("causallyBefore returns false when the first event is not locally known") {
    val (graph, _, _, genesis) = freshGraph()
    val unknown                = Hash.compute("unknown".getBytes)
    assert(!graph.causallyBefore(unknown, genesis.hash))
    assert(!graph.causallyAfter(genesis.hash, unknown))
  }

  test("concurrent holds for events on different branches even when their common ancestor is multiple hops back") {
    val (graph, holder, holderKey, genesis) = freshGraph()
    val branchB                             =
      buildEvent(DeltaCommitment(Hash.compute("b".getBytes)), holder, holderKey, Set(genesis.hash), genesis.hash)
    val graph1       = receiveOrFail(graph, branchB)
    val branchBChild =
      buildEvent(DeltaCommitment(Hash.compute("b-child".getBytes)), holder, holderKey, Set(branchB.hash), genesis.hash)
    val graph2 = receiveOrFail(graph1, branchBChild)

    val branchC =
      buildEvent(DeltaCommitment(Hash.compute("c".getBytes)), holder, holderKey, Set(genesis.hash), genesis.hash)
    val graph3       = receiveOrFail(graph2, branchC)
    val branchCChild =
      buildEvent(DeltaCommitment(Hash.compute("c-child".getBytes)), holder, holderKey, Set(branchC.hash), genesis.hash)
    val graph4 = receiveOrFail(graph3, branchCChild)

    assert(graph4.concurrent(branchBChild.hash, branchCChild.hash))
    assert(!graph4.causallyBefore(branchBChild.hash, branchCChild.hash))
    assert(!graph4.causallyBefore(branchCChild.hash, branchBChild.hash))
  }
}
