package lofi_acl.evaluation

import crypto.channels.PrivateIdentity
import lofi_acl.Debug
import lofi_acl.bft.{Acl, BftDelta}
import lofi_acl.sync.ChannelConnectionManager
import lofi_acl.sync.anti_entropy.AclEnforcingSync
import lofi_acl.sync.insecure.NonEnforcingSync
import lofi_acl.travelplanner.TravelPlan

import java.util.concurrent.{CountDownLatch, Executors}
import scala.util.Random

class MeshBenchmark {}

object MeshBenchmark {
  private val SEED                            = 42
  private val NUM_REPLICAS                    = 10
  private val NUM_DELTAS_PER_REPLICA          = 1_000
  private val MIN_ENTRIES_PER_MAP_PER_REPLICA = 10
  private val MAX_ENTRIES_PER_MAP_PER_REPLICA = 100

  def main(args: Array[String]): Unit =
    run(enforcementEnabled = true)

  def run(enforcementEnabled: Boolean): Unit = {
    given Random                     = Random(SEED)
    val startBenchmarkCountDownLatch = CountDownLatch(NUM_REPLICAS + 1)
    val endStateReachedLatch         = CountDownLatch(NUM_REPLICAS)

    val trace = TraceGeneration.generateTrace(
      NUM_REPLICAS,
      NUM_DELTAS_PER_REPLICA,
      MIN_ENTRIES_PER_MAP_PER_REPLICA,
      MAX_ENTRIES_PER_MAP_PER_REPLICA
    )
    val replicas = setup(trace, enforcementEnabled, endStateReachedLatch)

    println("ACL:\n" + Debug.shorten(replicas(0).sync.currentAcl).replace("|", "\n"))

    Thread.sleep(1_000)
    require(replicas.forall(_.sync.connectedPeers.size == 9))
    // println(replicas.map(r =>
    //   Debug.shorten(r.identity.getPublic) + ": " + r.sync.connectedPeers.toSeq.map(Debug.shorten).mkString(", ")
    // ).mkString("\n"))

    println("Starting benchmark")

    replayTrace(replicas, trace.deltas, startBenchmarkCountDownLatch)

    // Wait until all threads are ready
    startBenchmarkCountDownLatch.countDown()
    startBenchmarkCountDownLatch.await()

    val startTime = System.nanoTime()
    endStateReachedLatch.await()
    val endTime = System.nanoTime()

    println(s"All replicas converged after ${(endTime - startTime) / 1_000_000}ms")

    replicas.foreach(_.stop())
  }

  private def replayTrace(
      replicas: Array[MeshBenchmarkReplica],
      trace: Array[Array[TravelPlan]],
      startLatch: CountDownLatch
  ): Unit = {
    Executors.newVirtualThreadPerTaskExecutor()
    replicas.zipWithIndex
      .foreach((replica, idx) =>
        Thread.startVirtualThread(() =>
            startLatch.countDown()
            startLatch.await()
            replica.applyAllMutations(trace(idx))
        )
      )
  }

  private def setup(
      trace: Trace,
      enforcementEnabled: Boolean,
      finishedLatch: CountDownLatch
  ): Array[MeshBenchmarkReplica] = {
    val endState = trace.computeEndStateVersion(true)

    val replicas = trace.ids.map(id =>
      MeshBenchmarkReplica(
        "localhost",
        id,
        trace.genesis,
        enforcementEnabled,
        replica => if endState == replica.sync.stateVersion then finishedLatch.countDown()
      )
    )

    // Assign permissions
    replicas(0).sync.delegatePermission(trace.additionalPermissions)

    // Connect to root (should lead to all other replicas connecting to each other)
    replicas.foreach(_.start())
    replicas.foreach { replica =>
      replica.sync.connect(replicas(0).identity.getPublic, "localhost", replicas(0).sync.listenAddress.get._2)
    }

    replicas
  }
}

class MeshBenchmarkReplica(
    val ifAddress: String,
    val identity: PrivateIdentity,
    aclGenesis: BftDelta[Acl],
    enforcing: Boolean,
    onRdtChange: MeshBenchmarkReplica => Unit
) {
  val sync: AclEnforcingSync[TravelPlan] = {
    if enforcing then
        AclEnforcingSync[TravelPlan](
          identity,
          (id, recv) => ChannelConnectionManager(id, recv),
          aclGenesis,
          _ => onRdtChange(this)
        )
    else
        NonEnforcingSync[TravelPlan](
          identity,
          (id, recv) => ChannelConnectionManager(id, recv),
          aclGenesis,
          _ => onRdtChange(this)
        )
  }

  def applyAllMutations(trace: Array[TravelPlan]): Unit =
    trace.foreach(delta => sync.mutate(_ => delta))

  def start(): Unit = sync.start()

  def stop(): Unit = sync.stop()
}
