package rdts.documentation

class RDTs extends munit.FunSuite {

  /*:scim
= Algebraic Replicated Data Types
:flags = -hardwrap

This manual introduces Algebraic Replicated Data Types (ARDTs):
a systematic approach to building conflict-free replicated data
types by composition of product, and sum types.

The :m{merge} semantics of a compound data type are :b{automatically derived} from the merge semantics of its components.


• The chapter :ref{lattices} introduces :m{Lattice} and :m{Bottom} the algebraic foundation of all ARDTs.
• The chapter :ref{automatic-lattice-derivation} shows how merge semantics arise automatically from product and sum types.
• The chapter :ref{common-replicated-data-types} walks through the built-in data types that need no replica identity.
• The chapter :ref{data-types-with-identity} covers data types that use a replica id to track per-replica state.



This document contains compilable code, you can download the :link{full file; :path{RDTs.scim.scala}}.

# Lattices
:label = lattices

Every data type in this module is a :b{join-semilattice}.
A lattice for a type :m{A} is a trait with single operation:

```code lang=scala
  trait Lattice[A]:
    def merge(left: A, right: A): A
```

We usually will just import it
   */
  import rdts.base.Lattice
  /*:scim


The :m{merge} operation must be :b{associative}, :b{commutative}, and :b{idempotent}.
Merging two states yields a consistent result containing all information from both sides.

If :m{left `merge` right == right} then :m{left} is :m{subsumed} by :m{right}: merging :m{left} into :m{right} is a no-op.

## Lattices for existing types

To use a type as a lattice, we need to provide a :link{given instance; https://scala-lang.org/api/3.x/docs/contextual/givens.html} for that type. A :ref{safe} way to do this is to use an existing ordering:

   */
  given Lattice[Int] = Lattice.fromOrdering
  /*:scim

Merging based on an ordering will discard the smaller of the two values.

:note label=safe
  :i{Safe} in this case means that we must be sure that the lattce properties (associative, commutative, idempotent) are guaranteed.

The library comes with existing instances for built-in Scala types that you can just import.

   */
  import rdts.base.Lattice.given
  /*:scim

Now you can merge any two values of a type that has a :b{given} :m{Lattice} instance:

   */

  test("set lattice via union"):
      val setA: Set[Int] = Set(1, 2, 3)
      val setB: Set[Int] = Set(3, 4, 5)
      val mergedSet      = setA `merge` setB
      assertEquals(mergedSet, Set(1, 2, 3, 4, 5))

  /*:scim

Maps merge key-wise using the lattice of their values:

   */

  test("map lattice key-wise"):
      val mapA: Map[String, Int] = Map("x" -> 1, "y" -> 2)
      val mapB: Map[String, Int] = Map("y" -> 3, "z" -> 4)
      val mergedMap              = mapA `merge` mapB
      assertEquals(mergedMap("x"), 1)
      assertEquals(mergedMap("y"), 3)
      assertEquals(mergedMap("z"), 4)

  /*:scim

Options form a lattice where :m{None} is smaller than :m{Some}:

   */

  test("option lattice"):
      val optA: Option[Int] = None
      val optB: Option[Int] = Some(5)
      assertEquals(optA `merge` optB, Some(5))
      assertEquals(optB `merge` optA, Some(5))

  /*:scim

## Bottom

:m{Bottom[A]} provides the :b{empty} value of a lattice.
It is the identity element of merge:

```code
  Lattice.merge(Bottom.empty, x) == x
```

The empty set, the empty map, and :m{None} are all examples of bottom values.

# Automatic Lattice Derivation
:label = automatic-lattice-derivation

The core idea of ARDTs is that lattices can be :b{automatically derived} from the structure of data types.
No manual merge logic is needed.


:todo{find better place to import}
   */
  import rdts.datatypes.*
  /*:scim

## Product Lattices

For a :b{product} type like a case class, merge is :b{component-wise}.
Each field is merged independently using its own lattice.
You get the derivation with a single line:

   */

  test("product lattice derives component-wise merge"):
      val localId  = rdts.base.LocalUid.predefined("replica-alice")
      val remoteId = rdts.base.LocalUid.predefined("replica-bob")
      case class TwoCounters(a: GrowOnlyCounter, b: GrowOnlyCounter)
      given Lattice[TwoCounters]          = Lattice.derived
      given rdts.base.Bottom[TwoCounters] = rdts.base.Bottom.derived

      val tc1 = TwoCounters(GrowOnlyCounter.zero, GrowOnlyCounter.zero)
        .copy(a = GrowOnlyCounter.zero.inc()(using localId))

      val tc2 = TwoCounters(GrowOnlyCounter.zero, GrowOnlyCounter.zero)
        .copy(b = GrowOnlyCounter.zero.inc()(using remoteId))

      val merged = tc1 `merge` tc2
      assertEquals(merged.a.value, 1)
      assertEquals(merged.b.value, 1)

  /*:scim

The automatic derivation is why many data types only need :code{derives}:

```code
  case class PosNegCounter(pos: GrowOnlyCounter, neg: GrowOnlyCounter)
    derives Lattice, Bottom, Decompose
```

The derivation works for any nesting depth: a product of products, a product of sums, etc.

## Sum Lattices

For :b{sum types} (enums, sealed classes), later constructors are larger.
This models :b{state machines} that only move forward.

   */

  test("sum lattice for state machine"):
      enum Workflow:
          case Init()
          case Documents(hasStaffSheet: Boolean = false, hasHourConfirmation: Boolean = false)
          case Contract(signed: Boolean = false)
          case Complete()

      given Lattice[Workflow] = {
        given Lattice[Boolean]            = Lattice.fromOrdering
        given Lattice[Workflow.Init]      = Lattice.derived
        given Lattice[Workflow.Documents] = Lattice.derived
        given Lattice[Workflow.Contract]  = Lattice.derived
        given Lattice[Workflow.Complete]  = Lattice.derived
        Lattice.sumLattice
      }

      val wf0: Workflow = Workflow.Init()
      val wf1           = wf0 `merge` Workflow.Documents()
      assert(wf1.isInstanceOf[Workflow.Documents])

  /*:scim

The ordering is :m{Init < Documents < Contract < Complete}.
Once a workflow reaches :m{Complete}, merging any earlier state leaves it at :m{Complete}.

## Why Product and Sum?

Every data type can be expressed as a product of sums (or sum of products).
The derivation covers all data types, making the approach :b{algebraic}.

The underlying merge logic is straightforward:

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

# Common Replicated Data Types
:label = common-replicated-data-types

A Delta CRDT represents each mutation as a small :b{delta}, a lattice value that can be merged into a remote replica.

You apply operations to a data type and get back a delta.
Deltas are merged into a target state with :m{merge}.
The :m{DeltaBuffer} wrapper accumulates un-sent deltas for the middleware to ship.

This section covers the data types that need no replica identity: their operations carry enough information to be merged without knowing who created them.

## GrowOnlyCounter (GCounter)

An increment-only counter.
Internally it is a :b{product} of :m{Map[Uid, Int]}: each replica tracks its own contribution.
The counter value is the sum of all per-replica counts.

   */

  test("GrowOnlyCounter"):
      val localId  = rdts.base.LocalUid.predefined("replica-alice")
      val remoteId = rdts.base.LocalUid.predefined("replica-bob")

      var gCounter = GrowOnlyCounter.zero

      gCounter = gCounter `merge` gCounter.inc()(using localId)
      gCounter = gCounter `merge` gCounter.add(5)(using localId)

      assertEquals(gCounter.value, 6)

      gCounter = gCounter `merge` GrowOnlyCounter.zero.inc()(using remoteId)
      assertEquals(gCounter.value, 7)

  /*:scim

## PosNegCounter (PNCounter)

A counter for both increment and decrement.
It is a :b{product} of two :m{GrowOnlyCounter}s (positive and negative), merged component-wise.

   */

  test("PosNegCounter"):
      val localId = rdts.base.LocalUid.predefined("replica-alice")

      val pnCounter = PosNegCounter.zero

      val pn1 = pnCounter.add(5)(using localId)
      val pn2 = pnCounter.add(-3)(using localId)

      val mergedPN = pnCounter `merge` pn1 `merge` pn2
      assertEquals(mergedPN.value, 2)

  /*:scim

## GrowOnlySet (G-Set)

A set that only supports adding elements.  Merge is set union.

   */

  test("GrowOnlySet"):
      val gSet = GrowOnlySet.empty[String]

      val gs1 = gSet.add("apple")
      val gs2 = gSet.add("banana")

      val mergedGS = gSet `merge` gs1 `merge` gs2
      assertEquals(mergedGS.set, Set("apple", "banana"))

      val gs3       = gSet.add("apple")
      val mergedGS2 = mergedGS `merge` gs3
      assertEquals(mergedGS2.set, Set("apple", "banana"))

  /*:scim

## GrowOnlyList (G-List)

An append-only list that also supports insertion at an index.
Elements are ordered by causal timestamps.

   */

  test("GrowOnlyList"):
      val gList = GrowOnlyList.empty[String]

      val gl1 = gList.insertAfter(GrowOnlyList.headDot, "a")
      val gl2 = gl1.insertAfter(GrowOnlyList.headDot, "b")

      val mergedGL = gList `merge` gl1 `merge` gl2
      assertEquals(mergedGL.toList, List("b", "a"))

  /*:scim

# Data Types with Identity
:label = data-types-with-identity

Many data types need to know which replica is performing an operation.
The identity is provided as a :m{LocalUid} parameter.
This allows the data type to generate unique :m{Dot}s (globally unique points in time) for tracking causality.

## ReplicatedSet

A set supporting add and remove (Observed-Remove Set).
Removals do not override concurrent adds: the add wins.
The add operation needs a replica id to create a fresh dot.

   */

  test("ReplicatedSet"):
      val localId = rdts.base.LocalUid.predefined("replica-alice")

      var rSet = ReplicatedSet.empty[String]

      rSet = rSet `merge` rSet.add(using localId)("x")
      rSet = rSet `merge` rSet.add(using localId)("y")

      assertEquals(rSet.elements, Set("x", "y"))

      rSet = rSet `merge` rSet.remove("x")
      assertEquals(rSet.elements, Set("y"))

  /*:scim

## EnableWinsFlag (EWFlag)

A boolean flag where enable always wins over concurrent disable.
The enable operation allocates a fresh dot.

   */

  test("EnableWinsFlag"):
      val localId = rdts.base.LocalUid.predefined("replica-alice")

      val flag = EnableWinsFlag.empty

      val enabled = flag.enable(using localId)()
      assert(enabled.read == true)

      val dEnabled   = flag.disable()
      val dDisabled  = flag.enable(using localId)()
      val mergedFlag = dEnabled `merge` dDisabled
      assert(mergedFlag.read == true)

  /*:scim

## LastWriterWins (LWW Register)

A register that picks the write with the later causal timestamp.
On tie the payload lattice merges both values.

   */

  test("LastWriterWins"):
      val lww1 = LastWriterWins.now("hello")
      val lww2 = LastWriterWins.now("world")

      val mergedLWW = lww1 `merge` lww2
      assertEquals(mergedLWW.read, "world")

  /*:scim

## MultiVersionRegister (MV-Register)

A register that holds at most one value without concurrent writes, or all concurrently written values as a set.
Writing needs an id to create a fresh dot for the new value.

   */

  test("MultiVersionRegister"):
      val localId  = rdts.base.LocalUid.predefined("replica-alice")
      val remoteId = rdts.base.LocalUid.predefined("replica-bob")

      val mvReg = MultiVersionRegister.empty[String]

      val afterFirst = mvReg `merge` mvReg.write("first")(using localId)
      assertEquals(afterFirst.read, Set("first"))

      val afterConcurrent = afterFirst `merge` mvReg.write("second")(using remoteId)
      assertEquals(afterConcurrent.read, Set("first", "second"))

      val afterMerge = afterConcurrent `merge` afterConcurrent.write("third")(using localId)
      assertEquals(afterMerge.read, Set("third"))

  /*:scim

## ObserveRemoveMap (OR-Map)

A map with key-value semantics.  Removals are tracked via dots and do not override concurrent updates.

   */

  test("ObserveRemoveMap"):
      given Lattice[String] = Lattice.assertEquals
      val localId           = rdts.base.LocalUid.predefined("replica-alice")

      val orMap = ObserveRemoveMap.empty[String, String]

      val afterM1 = orMap `merge` orMap.update("key1", "value1")(using localId)
      assertEquals(afterM1.get("key1"), Some("value1"))

      val mRemove          = afterM1.remove("key1")
      val mAdd             = afterM1.update("key1", "value2")(using localId)
      val afterConcurrent2 = mRemove `merge` mAdd
      assertEquals(afterConcurrent2.get("key1"), Some("value2"))

  /*:scim

## ReplicatedList (RGA)

A replicated list with insert, update, and delete.
Uses a directed graph of dots for causal ordering.
All modifying operations need a replica id.

   */

  test("ReplicatedList"):
      val localId = rdts.base.LocalUid.predefined("replica-alice")

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
A higher epoch wins over a lower epoch.  Useful for wrapping types with weak merge semantics.

   */

  test("Epoch"):
      val localId = rdts.base.LocalUid.predefined("replica-alice")

      val epoch = Epoch.empty[GrowOnlyCounter]

      val ep1 = epoch.write(epoch.value.inc()(using localId))
      val ep2 = epoch.write(epoch.value.inc()(using localId))

      val mergedEpoch = epoch `merge` ep1 `merge` ep2
      assertEquals(mergedEpoch.read.value, 1)

  /*:scim

# Advanced Topics

## Decompose

:m{Decompose[A]} breaks a lattice value into smaller parts.
Merging all decomposed parts of :m{a} into :m{b} is equivalent to merging :m{a} directly into :m{b}.
This minimizes network transfer.

   */

  test("Decompose"):
      val localId = rdts.base.LocalUid.predefined("replica-alice")

      val gCounter2                             = GrowOnlyCounter.zero
      val dA                                    = gCounter2.inc()(using localId)
      val dB                                    = gCounter2.inc()(using localId)
      val decomposed: Iterable[GrowOnlyCounter] = dA.decomposed

      for part <- decomposed do
          assert(part.value <= dA.value)

  /*:scim

## Historized

:m{Historized[T]} detects when a new delta makes a previously buffered delta redundant, allowing middleware to discard it.

   */

  test("Historized"):
      import rdts.base.Historized.given

      val localId = rdts.base.LocalUid.predefined("replica-alice")

      val gCounter3 = GrowOnlyCounter.zero
      val histD1    = gCounter3.inc()(using localId)
      val histD2    = gCounter3.inc()(using localId)

      assert(histD2.isRedundant(histD1))

  /*:scim

## Decorated Lattice

:m{DecoratedLattice} wraps an existing lattice to add filtering and compaction.
Used internally by :m{ObserveRemoveMap}, :m{ReplicatedSet}, and :m{MultiVersionRegister} for observed-remove semantics.

## Causal Time

The module uses :m{Dot}s, globally unique pairs of :m{(Uid, Time)}, to track causality.
:m{Dots} is an efficient set of dots backed by :m{ArrayRanges} per replica.
:m{CausalTime} extends this with wall-clock milliseconds and a random tie-breaker for LWW registers.

## Protocols

The :m{rdts.protocols} package includes consensus protocols built on top of the delta CRDT foundation:

  • :m{Paxos}: multi-Paxos for state machine replication
  • :m{Voting}: simple voting protocol
  • :m{TwoPhaseCommit}: 2PC for atomic commitment
  • :m{Tokens}: token-based coordination

# Additional Resources

  • The :m{rdts.time} package implements vector clocks (:m{VectorClock}), interval tree clocks (:m{IntervalTreeClock}), and efficient dot storage (:m{Dots}).
  • The :m{rdts.filters} package provides permission-based filtering with :m{PermissionTree}.
  • For testing, see the :m{simulatedNetworkTests} which exercise full replication workflows.

The full API is documented in the scaladoc for each datatype.

   */
}
