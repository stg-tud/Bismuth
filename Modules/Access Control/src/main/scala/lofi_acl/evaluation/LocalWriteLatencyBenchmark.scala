package lofi_acl.evaluation

import crypto.PublicIdentity
import crypto.channels.{IdentityFactory, PrivateIdentity}
import lofi_acl.bft.*
import lofi_acl.bft.AclRdt.given_Encoder_BftDelta
import lofi_acl.sync.anti_entropy.AclEnforcingSync.encoder
import lofi_acl.sync.anti_entropy.{AclEnforcingSync, SignedDelta}
import lofi_acl.travelplanner.TravelPlan
import org.openjdk.jmh.annotations.*
import rdts.base.{LocalUid, Uid}
import rdts.filters.{Filter, PermissionTree}
import rdts.time.Dot

import java.util.concurrent.TimeUnit
import scala.util.Random

object ProfilerEntryPoint {
  def main(args: Array[String]): Unit = {
    val bench = LocalWriteLatencyBenchmark()

    val writeState = WriteState()
    0 until 100_000 foreach { _ => bench.decomposeFilterSign(writeState) }

    val readState = ReadState()
    0 until 100_000 foreach { _ => bench.verifyAndFilter(readState) }
  }
}

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Thread)
//@Warmup(iterations = 4, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
//@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
//@Fork(5)
//@Threads(1)
class LocalWriteLatencyBenchmark {

  @Benchmark
  def decomposeFilterSign(input: WriteState): Unit = {
    val localWritePermission = input.acl.write.getOrElse(input.localIdentity.getPublic, PermissionTree.empty)
    val delta                = {
      val delta = input.deltas(input.counter)
      input.counter = (input.counter + 1) % input.deltas.length
      delta
    }

    // We decompose the delta, filter it and then sign each individual part
    val decomposedVerifiedDeltas = delta.decomposed
      .filter(delta => Filter[TravelPlan].isAllowed(delta, localWritePermission)) // Enforce local write permissions
      .map(decomposedDelta => SignedDelta.fromDelta(input.localIdentity, Dot(input.uid, 42), decomposedDelta))
      .toSeq

    // Usually we would apply the update here, but for this benchmark, we don't
    // …

    // Now we filter with the receiving permissions
    val receiverReadPermission    = input.acl.read.getOrElse(input.remote, PermissionTree.empty)
    val (allowedDeltas, filtered) =
      decomposedVerifiedDeltas.partition(delta => Filter[TravelPlan].isAllowed(delta.payload, receiverReadPermission))

    // For now, let's ignore json serialization, as it is also required for non-enforcing
    // This is the message that we would be sent to the peer replica
    // writeToArray(DataDeltas(allowedDeltas, Dots.from(filtered.map(_.dot)), input.aclVersion))
  }

  @Benchmark
  def verifyAndFilter(input: ReadState): Unit = {
    val decomposedDeltas = {
      val delta = input.deltas(input.counter)
      input.counter = (input.counter + 1) % input.deltas.length
      delta
    }

    decomposedDeltas.filter(d =>
        val authorWritePerm = input.acl.write.getOrElse(PublicIdentity(d.dot.place.delegate), PermissionTree.empty)
        d.isSignatureValid
        && Filter[TravelPlan].isAllowed(d.payload, authorWritePerm)
    ): Unit
  }

}

@State(Scope.Thread)
class WriteState {
  var counter = 0

  val localIdentity: PrivateIdentity = IdentityFactory.createNewIdentity
  val local: PublicIdentity          = localIdentity.getPublic
  val uid                            = Uid(localIdentity.getPublic.id)

  val remote: PublicIdentity = IdentityFactory.createNewIdentity.getPublic

  val deltas: Array[TravelPlan]         = LocalWriteLatencyBenchmark.generateDeltas(using LocalUid(uid))
  val (acl: Acl, aclVersion: Set[Hash]) = LocalWriteLatencyBenchmark.getAcl(local, remote)
}

@State(Scope.Thread)
class ReadState {
  var counter = 0

  val authorIdentity: PrivateIdentity = IdentityFactory.createNewIdentity
  val author: PublicIdentity          = authorIdentity.getPublic
  val authorUid                       = Uid(authorIdentity.getPublic.id)

  val receiver: PublicIdentity = IdentityFactory.createNewIdentity.getPublic

  val deltas: Array[Seq[SignedDelta[TravelPlan]]] =
    LocalWriteLatencyBenchmark.generateDeltas(using LocalUid(authorUid)).map(d =>
      d.decomposed
        .map(decomposedDelta => SignedDelta.fromDelta(authorIdentity, Dot(authorUid, 42), decomposedDelta))
        .toSeq
    )
  val (acl: Acl, aclVersion: Set[Hash]) = LocalWriteLatencyBenchmark.getAcl(author, receiver)
}

object LocalWriteLatencyBenchmark {
  def getAcl(local: PublicIdentity, remote: PublicIdentity): (Acl, Set[Hash]) = {
    val delegation = Acl(
      read = Map(
        local  -> PermissionTree.allow,
        remote -> PermissionTree.fromPathSet(Set(
          "title",
          "bucketList",
          "expenses.inner.*.value.description",
          "expenses.inner.*.value.amount",
          "expenses.inner.*.dots",
          "expenses.removed"
        ))
      ),
      write = Map(
        local -> PermissionTree.fromPathSet(Set(
          "title",
          "bucketList",
          "expenses.inner.*.value.description",
          "expenses.inner.*.value.amount",
          "expenses.inner.*.dots",
          "expenses.removed"
          // "expenses.inner.*.value.comment"
        ))
      )
    )

    val root                                 = IdentityFactory.createNewIdentity
    val aclRdt                               = AclRdt(root)
    val genesis                              = AclRdt.createSelfSignedRoot(root)
    val hashDag: HashDag[BftDelta[Acl], Acl] = aclRdt.mutate(delegation, HashDag.fromRoot(genesis))

    (aclRdt.reconstruct(hashDag.heads, hashDag), hashDag.heads)
  }

  def generateDeltas(using localUid: LocalUid): Array[TravelPlan] = {
    given random: Random     = Random(42)
    var state                = TravelPlan.empty
    val bucketListEntryDelta = state.addBucketListEntry(dummy)
    val expenseDelta         = state.addExpense(dummy, "42.42€")
    state = state.merge(bucketListEntryDelta).merge(expenseDelta)

    val bucketListId = bucketListEntryDelta.bucketList.inner.keys.head
    val expenseId    = expenseDelta.expenses.inner.keys.head

    Array(
      state.setTitle(dummy),
      state.addBucketListEntry(dummy),
      state.removeBucketListEntry(bucketListId),
      state.setBucketListEntryText(bucketListId, dummy),
      state.addExpense(dummy, "42.42€"),
      state.removeExpense(expenseId),
      state.setExpenseAmount(expenseId, "21.21€"),
      state.setExpenseDescription(expenseId, dummy),
      state.setExpenseComment(expenseId, dummy),
    )
  }

  def dummy(using random: Random): String = random.alphanumeric.take(20).mkString("")
}
