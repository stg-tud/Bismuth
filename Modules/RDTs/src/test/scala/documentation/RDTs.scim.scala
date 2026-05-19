/*:scim
= Algebraic Replicated Data Types (ARDTs)
:flags = -hardwrap

This manual introduces Algebraic Replicated Data Types (ARDTs):
a systematic approach to building conflict-free replicated data
types from :b{lattice foundations}, :b{product types}, and
:b{sum types}.

The code examples are self-contained and executable.

• The chapter :ref{what-are-ardts} explains the core idea behind
  algebraic replicated data types.
• The chapter :ref{lattice-foundations} introduces :m{Lattice}
  and :m{Bottom}, the algebraic foundation of all RDTs.
• The chapter :ref{product-and-sum-lattices} shows how merge
  semantics are derived automatically from product and sum types.
• The chapter :ref{delta-crdts} explains how deltas build
  efficient replicated types.
• The chapter :ref{datatype-reference} presents the built-in
  data types one by one.
• The chapter :ref{advanced-topics} covers :b{Historized},
  :b{Decompose}, and consensus protocols.
 */

import rdts.base.Lattice
import rdts.base.Lattice.syntax.*
import rdts.datatypes.*
import rdts.syntax.DeltaBuffer

val localId = rdts.base.LocalUid.predefined("replica-alice")
val remoteId = rdts.base.LocalUid.predefined("replica-bob")

given Lattice[Int] = math.max
given Lattice[String] = Lattice.assertEquals

class RDTs extends munit.FunSuite:

/*:scim
# What are ARDTs?
:label = what-are-ardts

Algebraic Replicated Data Types (ARDTs) build :b{every data
type from product and sum types}.  The key insight is that the
:m{merge} semantics of a compound data type can be
:b{automatically derived} from the merge semantics of its
components.

For example, a :m{PosNegCounter} is a :b{product} of two
:m{GrowOnlyCounter}s.  The lattice derivation automatically
merges two :m{PosNegCounter} values by merging each component
independently.

Similarly, a workflow state machine can be modeled as a
:b{sum} of its states, where later constructors override
earlier ones.

# Lattice Foundations
:label = lattice-foundations

Every data type in this module is a :b{join-semilattice}.

```code
  trait Lattice[A]:
    def merge(left: A, right: A): A
```

The :m{merge} operation is :b{associative}, :b{commutative},
and :b{idempotent}.  Merging two conflicting states yields a
consistent result containing all information from both sides.

If :m{left `merge` right == right} then :m{left} is
:m{subsumed} by :m{right}: merging :m{left} into :m{right} is
a no-op.

## Bottom

:m{Bottom[A]} provides the :b{empty} value of a lattice,
the identity element of merge.

```code
  Lattice.merge(Bottom.empty, x) == x
```

## Lattice Instances for Standard Types

The module provides derived lattices for common Scala types:
*/

  test("set lattice via union"):
    val setA: Set[Int] = Set(1, 2, 3)
    val setB: Set[Int] = Set(3, 4, 5)
    val mergedSet      = setA `merge` setB
    assertEquals(mergedSet, Set(1, 2, 3, 4, 5))

/*:scim
Maps merge key-wise using the value lattice:
*/

  test("map lattice key-wise"):
    val mapA: Map[String, Int] = Map("x" -> 1, "y" -> 2)
    val mapB: Map[String, Int] = Map("y" -> 3, "z" -> 4)
    val mergedMap = mapA `merge` mapB
    assertEquals(mergedMap("x"), 1)
    assertEquals(mergedMap("y"), 3)
    assertEquals(mergedMap("z"), 4)

/*:scim
Options form a lattice where :m{None < Some}:
*/

  test("option lattice"):
    val optA: Option[Int] = None
    val optB: Option[Int] = Some(5)
    assertEquals((optA `merge` optB), Some(5))
    assertEquals((optB `merge` optA), Some(5))

/*:scim
# Product and Sum Lattices
:label = product-and-sum-lattices

The core idea of ARDTs: lattices can be :b{automatically
derived} for product types (case classes, tuples) and sum
types (enums, sealed traits).

## Product Lattices

For a product type like a case class, merge is :b{component-wise}.
Each field is merged independently using its own lattice.
*/

  test("product lattice derives component-wise merge"):
    case class TwoCounters(a: GrowOnlyCounter, b: GrowOnlyCounter)
    given Lattice[TwoCounters] = Lattice.derived
    given rdts.base.Bottom[TwoCounters] = rdts.base.Bottom.derived

    val tc1 = TwoCounters(GrowOnlyCounter.zero, GrowOnlyCounter.zero)
      .copy(a = GrowOnlyCounter.zero.inc()(using localId))

    val tc2 = TwoCounters(GrowOnlyCounter.zero, GrowOnlyCounter.zero)
      .copy(b = GrowOnlyCounter.zero.inc()(using remoteId))

    val merged = tc1 `merge` tc2
    assertEquals(merged.a.value, 1)
    assertEquals(merged.b.value, 1)

/*:scim
The automatic derivation is why many data types only need
a single line:

```code
  case class PosNegCounter(pos: GrowOnlyCounter, neg: GrowOnlyCounter)
    derives Lattice, Bottom, Decompose
```

## Sum Lattices

For :b{sum types} (enums, sealed classes), later constructors
are larger.  This models :b{state machines} that only move
forward.
*/

  test("sum lattice for state machine"):
    enum Workflow:
      case Init()
      case Documents(hasStaffSheet: Boolean = false, hasHourConfirmation: Boolean = false)
      case Contract(signed: Boolean = false)
      case Complete()

    given Lattice[Workflow] = {
      given Lattice[Boolean] = Lattice.fromOrdering
      given Lattice[Workflow.Init] = Lattice.derived
      given Lattice[Workflow.Documents] = Lattice.derived
      given Lattice[Workflow.Contract] = Lattice.derived
      given Lattice[Workflow.Complete] = Lattice.derived
      Lattice.sumLattice
    }

    val wf0: Workflow = Workflow.Init()
    val wf1 = wf0 `merge` Workflow.Documents()
    assert(wf1.isInstanceOf[Workflow.Documents])

/*:scim
The ordering is :m{Init < Documents < Contract < Complete}.
Once a workflow reaches :m{Complete}, merging any earlier
state leaves it at :m{Complete}.

## Why Product and Sum?

Every data type can be expressed as a product of sums
(or sum of products).  The derivation covers all possible
data types, making the approach :b{algebraic}.

The merge functions work as follows:

```code
  // merge for products: each field independently
  def mergeProduct(left: S, right: S): S =
    for each field i:
      result_i = lattice_i.merge(left_i, right_i)

  // merge for sums: larger constructor wins
  def mergeSum(left: T, right: T): T =
    if ordinal(left) < ordinal(right) then right
    else if ordinal(left) > ordinal(right) then left
    else lattice_for_constructor.merge(left, right)
```

# Delta CRDTs
:label = delta-crdts

A Delta CRDT represents each mutation as a small :b{delta},
a lattice value that can be merged into a remote replica to
bring it up to date.

The core workflow:

1. A replica modifies its local state and gets back a delta.
2. The delta is buffered for transmission.
3. The middleware ships deltas to other replicas.
4. The receiving replica merges the delta into its own state.

## DeltaBuffer

:m{DeltaBuffer[A]} wraps a state of type :m{A} (with a
:m{Lattice[A]}) and accumulates un-sent deltas.
*/

  test("delta buffer accumulates deltas on mod"):
    var counter: DeltaBuffer[GrowOnlyCounter] = DeltaBuffer(GrowOnlyCounter.zero)

    counter = counter.mod(_.inc()(using localId))

    assertEquals(counter.state.value, 1)
    assertEquals(counter.deltaBuffer.size, 1)

    counter = counter.clearDeltas()
    assert(counter.deltaBuffer.isEmpty)

  test("delta buffer accumulates multiple deltas"):
    var counter: DeltaBuffer[GrowOnlyCounter] = DeltaBuffer(GrowOnlyCounter.zero)

    counter = counter
      .mod(_.inc()(using localId))
      .mod(_.inc()(using localId))
      .mod(_.inc()(using localId))

    assertEquals(counter.state.value, 3)
    assertEquals(counter.deltaBuffer.size, 3)

/*:scim
## Merging Remote Deltas

Use :m{applyDelta} to merge a delta from another replica:
*/

  test("apply remote delta"):
    var counter: DeltaBuffer[GrowOnlyCounter] = DeltaBuffer(GrowOnlyCounter.zero)

    val deltaFromRemote = GrowOnlyCounter.zero.inc()(using remoteId)
    counter = counter.applyDelta(deltaFromRemote)

    assertEquals(counter.state.value, 1)
    assertEquals(counter.deltaBuffer.size, 1)

/*:scim
# Datatype Reference
:label = datatype-reference

## GrowOnlyCounter (GCounter)

An increment-only counter.  It is a :b{product} of
:m{Map[Uid, Int]}: each replica tracks its own contribution.
*/

  test("GrowOnlyCounter"):
    var gCounter = GrowOnlyCounter.zero

    gCounter = gCounter `merge` gCounter.inc()(using localId)
    gCounter = gCounter `merge` gCounter.add(5)(using localId)

    assertEquals(gCounter.value, 6)

    gCounter = gCounter `merge` GrowOnlyCounter.zero.inc()(using remoteId)
    assertEquals(gCounter.value, 7)

/*:scim
## PosNegCounter (PNCounter)

A counter supporting both increment and decrement.  It is a
:b{product} of two :m{GrowOnlyCounter}s (positive and
negative), merged component-wise.
*/

  test("PosNegCounter"):
    val pnCounter = PosNegCounter.zero

    val pn1 = pnCounter.add(5)(using localId)
    val pn2 = pnCounter.add(-3)(using localId)

    val mergedPN = pnCounter `merge` pn1 `merge` pn2
    assertEquals(mergedPN.value, 2)

/*:scim
## GrowOnlySet (G-Set)

A set that only supports adding elements.  Merge is union.
*/

  test("GrowOnlySet"):
    val gSet = GrowOnlySet.empty[String]

    val gs1 = gSet.add("apple")
    val gs2 = gSet.add("banana")

    val mergedGS = gSet `merge` gs1 `merge` gs2
    assertEquals(mergedGS.set, Set("apple", "banana"))

    val gs3 = gSet.add("apple")
    val mergedGS2 = mergedGS `merge` gs3
    assertEquals(mergedGS2.set, Set("apple", "banana"))

/*:scim
## ReplicatedSet

A set supporting add and remove (Observed-Remove Set).
Removals do not override concurrent adds: the add wins.
*/

  test("ReplicatedSet"):
    var rSet = ReplicatedSet.empty[String]

    rSet = rSet `merge` rSet.add(using localId)("x")
    rSet = rSet `merge` rSet.add(using localId)("y")

    assertEquals(rSet.elements, Set("x", "y"))

    rSet = rSet `merge` rSet.remove("x")
    assertEquals(rSet.elements, Set("y"))

/*:scim
## EnableWinsFlag (EWFlag)

A boolean flag where enable always wins over concurrent
disable.
*/

  test("EnableWinsFlag"):
    val flag = EnableWinsFlag.empty

    val enabled = flag.enable(using localId)()
    assert(enabled.read == true)

    val dEnabled  = flag.disable()
    val dDisabled = flag.enable(using localId)()
    val mergedFlag = dEnabled `merge` dDisabled
    assert(mergedFlag.read == true)

/*:scim
## LastWriterWins (LWW Register)

A register that picks the write with the later causal
timestamp.  On tie, the payload lattice merges both values.
*/

  test("LastWriterWins"):
    val lww1 = LastWriterWins.now("hello")
    val lww2 = LastWriterWins.now("world")

    val mergedLWW = lww1 `merge` lww2
    assertEquals(mergedLWW.read, "world")

/*:scim
## MultiVersionRegister (MV-Register)

A register that holds at most one value without concurrent
writes, or all concurrently written values as a set.
*/

  test("MultiVersionRegister"):
    val mvReg = MultiVersionRegister.empty[String]

    val afterFirst = mvReg `merge` mvReg.write("first")(using localId)
    assertEquals(afterFirst.read, Set("first"))

    val afterConcurrent = afterFirst `merge` mvReg.write("second")(using remoteId)
    assertEquals(afterConcurrent.read, Set("first", "second"))

    val afterMerge = afterConcurrent `merge` afterConcurrent.write("third")(using localId)
    assertEquals(afterMerge.read, Set("third"))

/*:scim
## ObserveRemoveMap (OR-Map)

A map with key-value semantics.  Removals are tracked via
dots and do not override concurrent updates.
*/

  test("ObserveRemoveMap"):
    val orMap = ObserveRemoveMap.empty[String, String]

    val afterM1 = orMap `merge` orMap.update("key1", "value1")(using localId)
    assertEquals(afterM1.get("key1"), Some("value1"))

    val mRemove  = afterM1.remove("key1")
    val mAdd     = afterM1.update("key1", "value2")(using localId)
    val afterConcurrent2 = mRemove `merge` mAdd
    assertEquals(afterConcurrent2.get("key1"), Some("value2"))

/*:scim
## GrowOnlyList (G-List)

An append-only list with insertion at an index.  Elements are
ordered by causal timestamps.
*/

  test("GrowOnlyList"):
    val gList = GrowOnlyList.empty[String]

    val gl1 = gList.insertAfter(GrowOnlyList.headDot, "a")
    val gl2 = gl1.insertAfter(GrowOnlyList.headDot, "b")

    val mergedGL = gList `merge` gl1 `merge` gl2
    assertEquals(mergedGL.toList, List("b", "a"))

/*:scim
## ReplicatedList (RGA)

A replicated list with insert, update, and delete.
Uses a directed graph of dots for causal ordering.
*/

  test("ReplicatedList"):
    var rList = ReplicatedList.empty[String]

    rList = rList `merge` rList.append("a")(using localId)
    rList = rList `merge` rList.append("b")(using localId)
    assertEquals(rList.toList, List("a", "b"))

    rList = rList `merge` rList.insertAt(1, "c")(using localId)
    assertEquals(rList.toList, List("a", "c", "b"))

    rList = rList `merge` rList.delete(1)
    assertEquals(rList.toList, List("a", "b"))

/*:scim
## Epoch Wrapper

:m{Epoch[E]} adds a monotonic counter to any datatype.
A higher epoch wins over a lower epoch.
*/

  test("Epoch"):
    val epoch = Epoch.empty[GrowOnlyCounter]

    val ep1 = epoch.write(epoch.value.inc()(using localId))
    val ep2 = epoch.write(epoch.value.inc()(using localId))

    val mergedEpoch = epoch `merge` ep1 `merge` ep2
    assertEquals(mergedEpoch.read.value, 1)

/*:scim
# Advanced Topics
:label = advanced-topics

## Decompose

:m{Decompose[A]} breaks a lattice value into smaller parts.
Merging all decomposed parts of :m{a} into :m{b} is equivalent
to merging :m{a} directly into :m{b}.
*/

  test("Decompose"):
    val gCounter2 = GrowOnlyCounter.zero
    val dA = gCounter2.inc()(using localId)
    val dB = gCounter2.inc()(using localId)
    val decomposed: Iterable[GrowOnlyCounter] = dA.decomposed

    for part <- decomposed do
      assert(part.value <= dA.value)

/*:scim
## Historized

:m{Historized[T]} detects when a new delta makes a previously
buffered delta redundant, allowing middleware to discard it.
*/

  test("Historized"):
    import rdts.base.Historized.given

    val gCounter3 = GrowOnlyCounter.zero
    val histD1 = gCounter3.inc()(using localId)
    val histD2 = gCounter3.inc()(using localId)

    assert(histD2.isRedundant(histD1))

/*:scim
## Decorated Lattice

:m{DecoratedLattice} wraps an existing lattice to add
filtering and compaction.  Used internally by
:m{ObserveRemoveMap}, :m{ReplicatedSet}, and
:m{MultiVersionRegister} for observed-remove semantics.

## Causal Time

The module uses :m{Dot}s, globally unique pairs of
:m{(Uid, Time)}, to track causality.  :m{Dots} is an
efficient set of dots backed by :m{ArrayRanges} per replica.

:m{CausalTime} extends this with wall-clock milliseconds and
a random tie-breaker for LWW registers.

## Protocols

The :m{rdts.protocols} package includes consensus protocols
built on top of the delta CRDT foundation:

• :m{Paxos}: multi-Paxos for state machine replication
• :m{Voting}: simple voting protocol
• :m{TwoPhaseCommit}: 2PC for atomic commitment
• :m{Tokens}: token-based coordination

# Additional Resources

• The :m{rdts.time} package implements vector clocks
  (:m{VectorClock}), interval tree clocks
  (:m{IntervalTreeClock}), and efficient dot storage
  (:m{Dots}).
• The :m{rdts.filters} package provides permission-based
  filtering with :m{PermissionTree}.
• For testing, see the :m{simulatedNetworkTests} which
  exercise full replication workflows.

The full API is documented in the Scaladoc for each datatype.
*/
