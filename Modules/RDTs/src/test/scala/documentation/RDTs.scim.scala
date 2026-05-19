package rdts.documentation

class RDTs extends munit.FunSuite {

  /*:scim
= Algebraic Replicated Data Types
:flags = -hardwrap

This manual introduces Algebraic Replicated Data Types (ARDTs):
a systematic approach to building conflict-free replicated data
types by composition of product, and sum types.

The :m{merge} semantics of a compound data type are :b{automatically derived} from the merge semantics of its components.

This document contains compilable code, you can download the :link{full file; :path{RDTs.scim.scala}}.

  • The chapter :ref{lattices} introduces :m{Lattice} and :m{Bottom} the algebraic foundation of all ARDTs.
  • The chapter :ref{automatic-lattice-derivation} shows how merge semantics arise automatically from product and sum types.
  • The chapter :ref{designing-replicated-data-types} explains identities and walks through implementing a GrowOnlyCounter from scratch.
  • The chapter :ref{common-replicated-data-types} guides through the built-in data types that need no replica identity.
  • The chapter :ref{data-types-with-identity} covers data types that use a replica id to track per-replica state.

# Lattices
:label = lattices

Every data type in this module is a :b{join-semilattice}.
A lattice for a type :m{A} is a trait with a single operation:

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
  :i{Safe} in this case means that we must be sure that the lattice properties (associative, commutative, idempotent) are guaranteed.

The library comes with existing instances for built-in Scala types that you can just import.

   */
  import rdts.base.Lattice.given
  import rdts.datatypes.*
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

## Product Lattices

For a :b{product} type like a case class, merge is :b{component-wise}.
Each field is merged independently using its own lattice.
You get the derivation with a single line:

   */

  test("product lattice derives component-wise merge"):
      given Lattice[Int] = math.max
      case class Data(counter: Int, items: Set[Int], mapping: Map[String, Int], flag: Option[Int])

      // automatically derive the lattice instance
      given Lattice[Data] = Lattice.derived

      val left  = Data(1, Set(1, 2), Map("a" -> 1), Some(1))
      val right = Data(2, Set(2, 3), Map("b" -> 2), None)

      val merged = left `merge` right
      assertEquals(merged.counter, 2)          // max(1, 2)
      assertEquals(merged.items, Set(1, 2, 3)) // union
      assertEquals(merged.mapping, Map("a" -> 1, "b" -> 2)) // key-wise merge
      assertEquals(merged.flag, Some(1)) // Some > None

  /*:scim

## Sum Lattices

For :b{sum types} (enums, sealed classes), we rely on their inherent total ordering to derive the lattice instance. Constructors that are defined later are considered larger.
Sum lattices are not as common as product lattices, but do allow modeling state machines.

   */

  test("sum lattice for state machine"):
      enum Workflow:
          case Init()
          case Documents(hasStaffSheet: Boolean = false, hasHourConfirmation: Boolean = false)
          case Contract(signed: Boolean = false)
          case Complete()

      given Lattice[Workflow] = {
        // each case of the sum is a Product type, so we need to derive lattices for each
        given Lattice[Boolean]            = Lattice.fromOrdering
        given Lattice[Workflow.Init]      = Lattice.derived
        given Lattice[Workflow.Documents] = Lattice.derived
        given Lattice[Workflow.Contract]  = Lattice.derived
        given Lattice[Workflow.Complete]  = Lattice.derived
        Lattice.sumLattice
      }

      val wf0: Workflow = Workflow.Init()
      val wf1           = wf0 `merge` Workflow.Documents()
      assertEquals(wf1, Workflow.Documents())

  /*:scim

The ordering is :m{Init < Documents < Contract < Complete}.
Once a workflow reaches :m{Complete}, merging any earlier state leaves it at :m{Complete}.

# Designing Replicated Data Types
:label = designing-replicated-data-types

Now that we can derive lattices from products and sums, we can design replicated data types.

The key motivation for ARDTs is that they provide :b{automatic convergence}:
no matter in which order replicas merge states, they always reach the same final
result.  This solves the hard problem of distributed convergence, but the resulting
value may not be :b{semantically useful}.

Consider counting votes in an election.  If we use a plain :m{Int},
two replicas each counting one vote would produce values :m{1} and :m{1}.
Merging with :code{max} gives :m{1}, losing a vote!  The replicas agree on
:m{1}, but the result is wrong.

ARDTs shift the problem from ensuring convergence (which is hard) to
designing good data types (which is much easier).
We need to design data types where concurrent operations do not overwrite each other.

A common trick is to use :b{replica identities}.  Each replica writes to
its own part of the state, so concurrent writes never conflict. The library provides a built in class to track unique IDs and the specific Uid of the local replica (which is a different type, to prevent accidential misuse).

   */
  import rdts.base.Uid
  import rdts.base.LocalUid
  import rdts.base.LocalUid.replicaId
  /*:scim

## A replicated counter

Let us design a counter that counts votes correctly.  Each replica tracks
its own contribution in a map from replica id to count.  The total is the
sum of all entries.  Since each replica only writes to its own key, no
information is lost on merge.



   */

  test("GrowOnlyCounter from scratch") {
    // A GCounter is just a map: each replica tracks its own count
    case class Counter(counters: Map[Uid, Int] = Map.empty):
        def value: Int = counters.values.sum

        // increment uses the local replica id to create a delta
        def inc()(using LocalUid): Counter =
          Counter(Map(replicaId -> (counters.getOrElse(replicaId, 0) + 1)))

    /*:scim

Note how the methods only return :b{delta states}: small values that need
to be merged into the state to take effect.

     */

    // We can derive the lattice automatically
    given Lattice[Counter] = Lattice.derived

    given LocalUid = LocalUid.predefined("replica-alice")

    var counter = Counter()

    // inc() returns a delta that we merge into the state
    counter = counter `merge` counter.inc()
    assertEquals(counter.value, 1)

    // merging deltas from multiple replicas adds their contributions
    val deltaAlice = counter.inc()
    val deltaBob   =
        given LocalUid = LocalUid.predefined("replica-bob")
        counter.inc()

    val total = counter `merge` deltaAlice `merge` deltaBob
    assertEquals(total.value, 3)
  }
  /*:scim

Each call to :m{inc()} produces a delta (a tiny :m{Counter} with just one entry).
Merging that delta into the state accumulates the value.
This is the essence of Delta RDTs: operations produce small lattice values that can be merged into any replica.

# Common Replicated Data Types
:label = common-replicated-data-types

This section covers the data types that need no replica identity beyond what we already saw:
their operations carry enough information to be merged without knowing who created them.

## GrowOnlyCounter (GCounter)

The library's built-in GrowOnlyCounter mirrors our implementation above.
It uses :m{Map[Uid, Int]} and provides :m{inc} and :m{add} operations.

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
This allows the data type to generate unique :m{Dot}s for tracking causality.

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
      assert(enabled.read == true, "enable should set flag")

      val dEnabled   = flag.disable()
      val dDisabled  = flag.enable(using localId)()
      val mergedFlag = dEnabled `merge` dDisabled
      assert(mergedFlag.read == true, "enable should win over disable")

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
      assert(afterM1.get("key1") == Some("value1"), "should have key1")

      val mRemove          = afterM1.remove("key1")
      val mAdd             = afterM1.update("key1", "value2")(using localId)
      val afterConcurrent2 = mRemove `merge` mAdd
      assert(afterConcurrent2.get("key1") == Some("value2"), "add should win over remove")

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
          assert(part.value <= dA.value, "decomposed part should be <= original")

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

      assert(histD2.isRedundant(histD1), "D2 should make D1 redundant")

  /*:scim

## Decorated Lattice

:m{DecoratedLattice} wraps an existing lattice to add filtering and compaction.
Used internally by :m{ObserveRemoveMap}, :m{ReplicatedSet}, and :m{MultiVersionRegister} for observed-remove semantics.

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
