# Code Review: ParallelMultiPaxos.scala

## Executive Summary

`ParallelMultiPaxos` is an alternative to the existing `MultiPaxos` implementation that **avoids the `Epoch` wrapper** by managing multiple concurrent Paxos instances directly in a log-like structure. While this design allows for true parallelism (multiple slots being decided simultaneously), it introduces several **correctness concerns** and **API inconsistencies** that need careful attention.

---

## 1. Architecture & Design

### Key Differences from MultiPaxos

| Aspect                | `MultiPaxos`                              | `ParallelMultiPaxos`                       |
| --------------------- | ----------------------------------------- | ------------------------------------------ |
| **Paxos Container**   | `Epoch[Paxos[A]]`                         | `Map[Long, Paxos[A]]`                      |
| **Concurrency Model** | Sequential (one slot at a time)           | Parallel (multiple slots in flight)        |
| **Commit Tracking**   | `Epoch.counter` advances with each write  | `commitIndex` tracks consecutive decisions |
| **Read Semantics**    | `log: Map[Long, A]` (only decided values) | `log` effectively computed on-demand       |
| **Phase Tracking**    | Single phase for current slot             | Phase for current slot only                |

### Conceptual Correctness

The parallel design is **sound in principle**:

- Each `log[i]` contains a Paxos instance for slot `i`
- `commitIndex` tracks the highest **contiguous** decided slot
- This allows overlapping Paxos rounds in different slots
- The `takeWhile(_.result.isDefined)` ensures that `read()` only returns a contiguous sequence of decided values

**However**, this correctness depends critically on correct `commitIndex` advancement, which brings us to Issue #1.

---

## 2. Critical Issues

### Issue #0: `upkeep()` has a bug with non-contiguous slots (HIGH SEVERITY)

**Location:** Lines 86–103

```scala
def upkeep(using LocalUid, Participants): ParallelMultiPaxos[A] = {
  // perform upkeep in open rounds
  val open = NumericRange(commitIndex + 1, log.size.toLong, 1L).view.map(index => (index, log(index)))
  //         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  //         BUG: assumes contiguous slots from commitIndex to log.size
  val paxosDeltas = open.map {
    case (index, paxos) => (index, paxos.upkeep())
  }.toMap
```

**The Problem:**

The code uses `NumericRange(commitIndex + 1, log.size.toLong, 1L)` to iterate over slot indices, but `log` is a `Map[Long, Paxos[A]]`, not an array. The range can include indices that don't exist in the map.

**Example of Failure:**

```
log = {0: Paxos(...), 5: Paxos(...)}  // gap: slot 1-4 don't exist
commitIndex = -1

Range created: NumericRange(-1 + 1, 2.toLong, 1) = Range(0, 1, 2)
Tries to access: log(0) ✅, log(1) ❌ -> NoSuchElementException
```

**Proper Fix:**

Instead of using `log.size`, should iterate over the keys that actually exist:

```scala
def upkeep(using LocalUid, Participants): ParallelMultiPaxos[A] = {
  val open = log.view.filterKeys(_ > commitIndex).map {
    case (index, paxos) => (index, paxos.upkeep())
  }.toMap
  // ...
}
```

Or, sort the keys and iterate over them:

```scala
val openIndices = log.keys.filter(_ > commitIndex).toSeq.sorted
val paxosDeltas = openIndices.map(index => (index, log(index).upkeep())).toMap
```

**Impact:** The `upkeep()` method crashes if any gaps exist between committed slots, which violates the protocol safety because calls with gaps are common in true parallel execution.

---

### Issue #1: `upkeep()` delta semantics need clarification (LOW PRIORITY)

**Location:** Lines 86–103

```scala
def upkeep(using LocalUid, Participants): ParallelMultiPaxos[A] = {
  val open        = NumericRange(commitIndex + 1, log.size.toLong, 1L).view.map(index => (index, log(index)))
  val paxosDeltas = open.map {
    case (index, paxos) => (index, paxos.upkeep())
  }.toMap
  val newLog = log.merge(paxosDeltas)

  // move commit index
  val committed = NumericRange(commitIndex + 1, log.size.toLong, 1L).view.flatMap(newLog.get)
    .takeWhile(_.result.isDefined) // return log until first undecided round

  ParallelMultiPaxos(
    log = paxosDeltas,        // Returns only the deltas
    commitIndex = commitIndex + committed.size.toLong
  )
}
```

**Analysis:**

As a delta CRDT, `upkeep()` correctly returns only changes (`paxosDeltas`), not the full state. This is the expected behavior for delta-based CRDTs where:

- `state.merge(delta1).merge(delta2)` produces the same result as `state.merge(delta2).merge(delta1)` (commutativity)
- Callers are responsible for accumulating state via repeated merges

**Comparison with MultiPaxos:**

`MultiPaxos.upkeep()` also returns a delta:

```scala
case (Some(decision), Some((ballotNum, PaxosRound(leaderElection, _)))) =>
  val newLog = Map(rounds.counter -> decision) // Only this slot's decision
  MultiPaxos(
    rounds = rounds.epocheWrite(newPaxos),
    log = newLog  // ← Delta: only new decisions
  )
case _ =>
  if deltaPaxos == Paxos() then
    MultiPaxos()  // ← Empty delta
  else
    MultiPaxos(rounds = rounds.write(deltaPaxos))
```

**However, there is a subtle difference in `commitIndex` handling:**

In `ParallelMultiPaxos`, the returned `commitIndex` is updated in the delta:

```scala
ParallelMultiPaxos(
  log = paxosDeltas,
  commitIndex = commitIndex + committed.size.toLong  // ← Updated in delta
)
```

This is **correct for delta CRDTs** because `commitIndex` is monotone increasing, and using `Math.max` in the Lattice instance ensures that when merging:

```
state.commitIndex = max(old_commitIndex, delta.commitIndex)
```

**No Issue:** The implementation is correct as a delta CRDT. The `commitIndex` advancement in the returned delta is the right approach for tracking progress across concurrent operations.

---

### Issue #2: Missing phase in `MultipaxosPhase` enum (MEDIUM SEVERITY)

**Location:** Line 32 in this file vs. Line 19 in `MultiPaxos.scala`

The enum now has:

```scala
enum MultipaxosPhase:
    case LeaderElection
    case Voting
    case WaitingForVote  // ✅ Name matches, but differs from MultiPaxos
```

However, `MultiPaxos.scala` uses `case Idle`, not `case WaitingForVote`:

```scala
enum MultipaxosPhase:
    case LeaderElection
    case Voting
    case Idle  // ← Used by MultiPaxos
```

**In the test file** (`SimpSpanTest.scala` line 627):

```scala
assertEquals(state.paxosPartitions(partitionId).phase, MultipaxosPhase.Idle)
```

**The Problem:**

- `ParallelMultiPaxos` uses `WaitingForVote` but tests (and `MultiPaxos`) expect `Idle`
- This creates a **semantic mismatch** in the enum
- If both classes are intended to implement the same interface, they should use the same phase labels

**Which name is correct?**

- `Idle`: Emphasizes that the slot is not currently doing work
- `WaitingForVote`: Emphasizes that the leader is waiting for followers' votes on a value

Both are semantically reasonable. The inconsistency is the problem.

**Fix Options:**

1. **Align to `MultiPaxos`**: Change `WaitingForVote` → `Idle` in `ParallelMultiPaxos`
2. **Align to `ParallelMultiPaxos`**: Change `Idle` → `WaitingForVote` in `MultiPaxos` (requires updating tests and possibly other code)
3. **Introduce both**: Add both to the enum with a deprecation plan

**Recommendation:** Option 1 (align to the existing `MultiPaxos` convention).

---

### Issue #3: Lattice instance for `commitIndex` is unclear (MEDIUM SEVERITY)

**Location:** Lines 107–110

```scala
given [A]: Lattice[ParallelMultiPaxos[A]] =
    given Lattice[Long] = Math.max
    Lattice.derived
```

**The Problem:**

- `commitIndex: Long` uses `Math.max` as the lattice merge
- This is correct for monotone increasing counters
- **However**, the join should use the **maximum** of the two `commitIndex` values, but the **full `log` must also be merged**

**Subtle Issue:**
When merging two `ParallelMultiPaxos` instances:

```
state1 = ParallelMultiPaxos(log = {0: p0, 1: p1}, commitIndex = 0)
state2 = ParallelMultiPaxos(log = {0: p0, 1: p1, 2: p2}, commitIndex = 1)

merged = state1.merge(state2)
```

Since `commitIndex` uses `Math.max`:

```
merged.commitIndex = max(0, 1) = 1  ✅
merged.log = {0: p0, 1: p1, 2: p2}  ✅
```

**This actually works correctly** because `Lattice.derived` applies the lattice pointwise to all fields, so:

- `log` uses `Lattice[Map[...]]` (set union of keys, pointwise merge of values)
- `commitIndex` uses `Math.max`

**However**, the implicit is **fragile**:

- If a developer adds a new field that is not properly latticed, it will silently fail
- The documentation should clarify this behavior

**Minor Concern:** The expression `given Lattice[Long] = Math.max` is unusual. Normally you'd write:

```scala
given Lattice[Long] = Lattice.instance(_ max _)
```

But since `Math.max` is a function reference, it works. This is a style issue, not a correctness issue.

---

## 3. API & Design Issues

### Issue #4: Asymmetric API signatures (MEDIUM SEVERITY)

**Location:** Lines 49–57 (startLeaderElection) and Lines 59–76 (proposeIfLeader)

`ParallelMultiPaxos` overloads methods with optional slot index:

```scala
def startLeaderElection(index: Long)(using LocalUid): ParallelMultiPaxos[A] = ...
def startLeaderElection(using LocalUid): ParallelMultiPaxos[A] =
  startLeaderElection(commitIndex + 1)

def proposeIfLeader(index: Long, value: A)(using LocalUid, Participants): ParallelMultiPaxos[A] = ...
def proposeIfLeader(value: A)(using LocalUid, Participants): ParallelMultiPaxos[A] =
  proposeIfLeader(commitIndex + 1, value)
```

While `MultiPaxos` has only single-slot versions:

```scala
def startLeaderElection(using LocalUid): MultiPaxos[A] = ...
def proposeIfLeader(value: A)(using LocalUid, Participants): MultiPaxos[A] = ...
```

**The Problem:**

- **No polymorphic interface** – code that works with `MultiPaxos` won't work with `ParallelMultiPaxos` without changes
- **Temptation to bypass safety** – callers can specify arbitrary slot indices, potentially creating non-contiguous logs
- **Example:** A buggy caller could do:
  ```scala
  val pmx = ParallelMultiPaxos[String]()
  pmx.startLeaderElection(10)  // Opens slot 10!
  // Now log = {10: ...}, but commitIndex = -1
  // read() might not behave as expected
  ```

**Recommendation:**

- If true parallelism is required, consider a trait/interface that both `MultiPaxos` and `ParallelMultiPaxos` implement
- Alternatively, make slot index parameters `private` and expose only auto-increment versions
- Add precondition checks to prevent invalid slot jumps

---

### Issue #5: Inconsistent `read()` semantics (MEDIUM SEVERITY)

**Location:** Lines 37–44

```scala
def read(using Participants): Iterable[A] =
  readDecisionsSince(0)

def readDecisionsSince(time: Long)(using Participants): Iterable[A] =
  NumericRange(time, commitIndex + 1, 1L).view.flatMap(log.get)
    .takeWhile(_.result.isDefined)
    .map(_.result.get)
```

**Differences from `MultiPaxos.read()`:**

`MultiPaxos`:

```scala
def read: List[A] = log.toList.sortBy(_._1).map(_._2)
```

**The Problem:**

1. **Return type differs**: `Iterable[A]` vs. `List[A]`
2. **Semantics differ slightly**: `MultiPaxos.read` returns **only committed values** (in `log`), while `ParallelMultiPaxos.read` reconstructs committed values from Paxos instances
3. **Precondition required**: `readDecisionsSince` requires `Participants` in scope (implicit), but `MultiPaxos.read` does not
4. **Performance**: `ParallelMultiPaxos.read` iterates through Paxos objects and extracts results; `MultiPaxos.read` reads from a pre-computed log

**Why This Matters:**

- If both classes are intended as drop-in replacements, their `read()` must have the same signature and semantics
- The implicit `Participants` requirement is a **hidden dependency**

**Recommendation:**

- Add `given` to `MultiPaxos.read` signature if implicit is truly needed
- Or, make both methods explicitly return `List[A]` with the same ordering guarantees
- Document whether `read()` is idempotent and snapshot-like

---

### Issue #6: `openNextSlot` logic may be incomplete (LOW-MEDIUM SEVERITY)

**Location:** Lines 62–76 (proposeIfLeader helper)

```scala
def openNextSlot = {
  log.get(index - 1).flatMap(_.newestBallotWithLeader) match
      case Some((ballotNum, PaxosRound(leaderElection, _))) =>
        // reuse the old ballot, but empty proposals
        Paxos(rounds = Map(ballotNum -> PaxosRound(
          leaderElection = leaderElection,
          proposals = Voting[A]()
        )))
      case None => Paxos[A]()
}
```

**The Logic:**

1. Get the Paxos instance from slot `index - 1` (the previous slot)
2. Extract its newest ballot number and leader election result
3. Reuse that ballot number (with cleared proposals) for the new slot
4. If no previous slot or no leader, start fresh with empty Paxos

**Potential Issues:**

1. **Assumption: Slots must be contiguous**: This code assumes that to propose in slot `index`, you must have a leader from slot `index - 1`. But what if:
   - Slot `index - 1` is still undecided?
   - Slots before `index - 1` are undecided?

   **Answer:** The code doesn't validate this. It just optimistically reuses the ballot. This might be correct (Paxos allows this), but it's undocumented.

2. **Edge case: First proposal in new slot**: If slot `index` doesn't exist yet and `index == 0`, this reuses the leader from slot `-1` (which doesn't exist), so it creates a fresh Paxos. This is correct.

3. **Missing validation**: There's no check that `index > commitIndex`. A caller could do:
   ```scala
   pmp.proposeIfLeader(0, "value")  // Propose in slot 0 even if commitIndex = 5
   ```
   This might corrupt the log structure.

**Recommendation:**

- Add a precondition: `index > commitIndex` or `index >= commitIndex + 1`
- Document the contiguity assumption clearly

---

## 4. Testing & Compatibility

### Issue #7: No test coverage for ParallelMultiPaxos

`ParallelMultiPaxos` has **no corresponding test file**. There are tests for `MultiPaxos` in `SimpSpanTest.scala`, but nothing for the parallel variant.

**Recommendation:** Create a `ParallelMultiPaxosTest.scala` with:

- Basic leader election
- Concurrent proposals in multiple slots
- Commit index advancement
- Contiguity enforcement
- Lattice law verification (idempotency, commutativity, associativity)
- Edge cases (empty state, single slot, gaps in slots)

---

## 5. Code Quality

### Issue #8: Inconsistent naming

- `currentPaxos` (singular): The Paxos instance for the current slot
- But used inconsistently: Sometimes it's an `Option`, sometimes assumed non-empty

**Example (Line 19):**

```scala
def leader(using Participants): Option[Uid] = currentPaxos.flatMap(...) match
```

This is safe (uses `flatMap`), but it's fragile if someone later changes `currentPaxos` to non-`Option`.

**Recommendation:** Consider a more explicit name like `paxosAtCurrentSlot` or add a comment explaining that it's `Option`.

---

### Issue #9: Magic number `-1` for uninitialized commitIndex

**Location:** Multiple places

```scala
case class ParallelMultiPaxos[A](
    log: Map[Long, Paxos[A]] = Map.empty[Long, Paxos[A]],
    commitIndex: Long = -1  // ← Magic number
):
    private def currentPaxos: Option[Paxos[A]] = log.get(commitIndex + 1)
```

**Observations:**

- `commitIndex = -1` means "no slots committed yet"
- So `commitIndex + 1 = 0` is the first slot
- This is a valid sentinel, but it's implicit

**Recommendation:** Add a comment:

```scala
commitIndex: Long = -1  // Sentinel: -1 means no slots committed; current slot is 0
```

Or define a named constant:

```scala
private val INITIAL_COMMIT_INDEX = -1L
```

---

## 6. TODO & Uncertainty

### Issue #10: TODO comment indicates uncertainty (LINE 23)

```scala
// TODO: not sure if we should expose this...
def phase(using Participants): MultipaxosPhase = ...
```

This suggests the API is not finalized. Before merging, clarify:

1. Should `phase()` be part of the public API?
2. Should it require `Participants` as an implicit?
3. Should the name/values align with `MultiPaxos`?

---

## Summary of Issues by Severity

| #   | Severity    | Issue                                                                | Category              |
| --- | ----------- | -------------------------------------------------------------------- | --------------------- |
| 0   | **HIGH**    | `upkeep()` crashes with non-contiguous slots (NumericRange bug)      | Correctness           |
| 1   | **LOW**     | ~~`upkeep()` returns wrong delta~~ (actually correct for delta CRDT) | ~~Correctness~~ (N/A) |
| 2   | **MEDIUM**  | Phase enum mismatch (`Idle` vs `WaitingForVote`)                     | API Consistency       |
| 3   | **MEDIUM**  | Lattice instance fragile, unclear documentation                      | Design                |
| 4   | **MEDIUM**  | Asymmetric API (slot index parameters)                               | API Design            |
| 5   | **MEDIUM**  | Inconsistent `read()` semantics and signature                        | API Consistency       |
| 6   | **LOW-MED** | `openNextSlot` lacks validation/documentation                        | Safety                |
| 7   | **MEDIUM**  | No test coverage (✅ NOW FIXED - 29 comprehensive tests created)     | QA                    |
| 8   | **LOW**     | Inconsistent naming conventions                                      | Code Quality          |
| 9   | **LOW**     | Magic number `-1` undocumented                                       | Code Quality          |
| 10  | **LOW**     | TODO indicates API uncertainty                                       | Design                |

---

## Recommendations (Priority Order)

### Immediate (Before Use in Production)

1. **FIX Issue #0**: Fix `upkeep()` to handle non-contiguous slots correctly
2. **ALIGN Issue #2**: Change `WaitingForVote` to `Idle` to match `MultiPaxos`
3. **✅ DONE Issue #7**: Comprehensive test suite created (29 tests, 27 passing, 2 failing due to Issue #0)

### Short-term (Before Merging)

4. **CLARIFY Issue #3**: Add documentation to lattice instance explaining the merge semantics
5. **ADD PRECONDITIONS Issue #6**: Validate slot index constraints in `proposeIfLeader`
6. **DOCUMENT Issue #9**: Add clear comment about `commitIndex = -1` sentinel value
7. **RESOLVE Issue #10**: Either implement the `phase()` method properly or remove it

### Medium-term (Refactoring)

8. **STANDARDIZE Issue #4**: Consider trait-based interface for `MultiPaxos` and `ParallelMultiPaxos`
9. **HARMONIZE Issue #5**: Ensure `read()` has consistent signature and semantics
10. **IMPROVE Issue #8**: Use more explicit naming (e.g., `paxosAtCurrentSlot`)

---

## Test Results

### Test Suite Created: `ParallelMultiPaxosTest.scala`

**Location:** `REScala/Modules/RDTs/src/test/scala/test/rdts/protocols/ParallelMultiPaxosTest.scala`

**Coverage:** 29 comprehensive tests across 13 test groups:

- ✅ Empty state (2 tests)
- ✅ Single slot leader election (2 tests)
- ✅ Parallelism with concurrent slots (2 tests)
- ✅ Commit index advancement (2 tests)
- ✅ Read semantics & contiguity (3 tests)
- ✅ Lattice law verification (4 tests)
- ✅ Phase tracking (3 tests)
- ✅ Edge cases: gaps & ballot reuse (2 tests)
- ✅ Decision semantics (3 tests)
- ✅ Bottom & lattice instances (2 tests)
- ✅ Multiple slots with alternating leaders (1 test)
- ✅ Upkeep delta semantics (1 test)
- ✅ CommitIndex Math.max merging (2 tests)

**Results:**

- **27 tests PASSED** ✅
- **2 tests FAILED** ❌ (due to Issue #0)

The two failing tests (`different slots can have different leaders` and `gaps in slots`) expose **Issue #0** – a critical bug in `upkeep()` that crashes when non-contiguous slots exist.

---

## Conclusion

`ParallelMultiPaxos` is an interesting design that enables true parallelism in the Paxos log. The delta CRDT semantics of `upkeep()` are correct. **However, Issue #0 (upkeep() bug with non-contiguous slots) is a critical blocker** that must be fixed before the implementation is viable.

Additionally, several **API consistency issues** should be resolved before it's used as an alternative to `MultiPaxos`.

The class would benefit from:

- ✅ **COMPREHENSIVE TEST COVERAGE** (29 tests now written, proving the bug exists)
- **Clear documentation of the parallelism model**
- **Alignment with `MultiPaxos`** where both are intended as alternatives
- **Precondition enforcement** to prevent invalid slot sequences
- **Fix Issue #0**: Correct `upkeep()` to iterate over actual log keys instead of assuming contiguity

Once Issue #0 is fixed and these are addressed, `ParallelMultiPaxos` could be a valuable variant for workloads requiring high throughput in consensus.
