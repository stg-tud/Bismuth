package lofi_acl.evaluation

import crypto.channels.PrivateIdentity
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
  private val NUM_DELTAS_PER_REPLICA          = 10_000
  private val MIN_ENTRIES_PER_MAP_PER_REPLICA = 10
  private val MAX_ENTRIES_PER_MAP_PER_REPLICA = 100
  private val NUM_REPETITIONS                 = 10

  def main(args: Array[String]): Unit = {
    given Random     = Random(SEED)
    val trace: Trace = TraceGeneration.generateTrace(
      NUM_REPLICAS,
      NUM_DELTAS_PER_REPLICA,
      MIN_ENTRIES_PER_MAP_PER_REPLICA,
      MAX_ENTRIES_PER_MAP_PER_REPLICA
    )

    val results = (0 until NUM_REPETITIONS * 2).map(i =>
        val enforcementEnabled = i % 2 == 0
        val runtimeNs          = run(enforcementEnabled, trace)
        println(
          s"Full mesh [$i/${NUM_REPETITIONS * 2}]: enforcement=$enforcementEnabled, runtime_ms=${runtimeNs / 1_000_000}"
        )
        enforcementEnabled -> runtimeNs
    )

    println("replicas,deltas_per_replica,enforcing,runtime_ns")
    results
      .foreach((enforcementEnabled, runtimeNs) =>
        // replicas,deltas_per_replica,enforcing,runtime_ns
        println(s"$NUM_REPLICAS,$NUM_DELTAS_PER_REPLICA,$enforcementEnabled,$runtimeNs")
      )

  }

  def run(enforcementEnabled: Boolean, trace: Trace): Long = {
    val startBenchmarkCountDownLatch = CountDownLatch(NUM_REPLICAS + 1)
    val endStateReachedLatch         = CountDownLatch(NUM_REPLICAS)

    val replicas = setup(trace, enforcementEnabled, endStateReachedLatch)

    Thread.sleep(100) // Give the replicas some time to connect to each other and process all messages
    // Make sure that all replicas are connected to all others
    require(replicas.forall(_.sync.connectedPeers.size == NUM_REPLICAS - 1))

    // Start benchmark (waits until all are ready)
    replayTrace(replicas, trace.deltas, startBenchmarkCountDownLatch)

    // Wait until all threads are ready
    startBenchmarkCountDownLatch.countDown()
    startBenchmarkCountDownLatch.await()

    val startTime = System.nanoTime()
    endStateReachedLatch.await()
    val endTime = System.nanoTime()

    replicas.foreach(_.stop())

    endTime - startTime
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
            startLatch.countDown() // Signal ready to start
            startLatch.await()     // Wait for all other threads before starting
            replica.applyAllMutations(trace(idx))
        )
      )
  }

  private def setup(
      trace: Trace,
      enforcementEnabled: Boolean,
      finishedLatch: CountDownLatch
  ): Array[MeshBenchmarkReplica] = {
    val endStateVersion = trace.computeEndStateVersion(enforcementEnabled)

    val replicas = trace.ids.map(id =>
      MeshBenchmarkReplica(
        "localhost",
        id,
        trace.genesis,
        enforcementEnabled,
        replica =>
          if endStateVersion == replica.sync.stateVersion
          then finishedLatch.countDown()
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
