package lofi_acl.evaluation

import crypto.channels.IdentityFactory
import lofi_acl.travelplanner.TravelPlan
import rdts.filters.PermissionTree

import java.util.concurrent.{CountDownLatch, Executors}
import scala.util.Random

object SimulationBenchmark {
  // TODO: specify total num updates instead of per replica?
  private val SEED                            = 42
  private val NUM_REPLICAS                    = 10
  private val NUM_DELTAS_TOTAL                = 10_000 * NUM_REPLICAS
  private val MIN_ENTRIES_PER_MAP_PER_REPLICA = 10
  private val MAX_ENTRIES_PER_MAP_PER_REPLICA = 100
  private val NUM_REPETITIONS                 = 10

  def main(args: Array[String]): Unit = {
    require(NUM_DELTAS_TOTAL / NUM_REPLICAS * NUM_REPLICAS == NUM_DELTAS_TOTAL)
    given Random     = Random(SEED)
    val trace: Trace = TraceGeneration.generateTrace(
      NUM_REPLICAS,
      NUM_DELTAS_TOTAL / NUM_REPLICAS,
      MIN_ENTRIES_PER_MAP_PER_REPLICA,
      MAX_ENTRIES_PER_MAP_PER_REPLICA
    )

    val resultsP2p = (0 until NUM_REPETITIONS * 2).map(i =>
        val enforcementEnabled = i % 2 == 0
        val runtimeNs          = benchmarkP2p(enforcementEnabled, trace)
        println(
          s"Full mesh [${i + 1}/${NUM_REPETITIONS * 2}]: enforcement=$enforcementEnabled, runtime_ms=${runtimeNs / 1_000_000}"
        )
        (enforcementEnabled, "p2p", runtimeNs)
    )
    val resultsCentralized = (0 until NUM_REPETITIONS).map(i =>
        val runtimeNs = benchmarkCentralized(trace)
        println(s"Centralized [${i + 1}/$NUM_REPETITIONS]: runtime_ms=${runtimeNs / 1_000_000}")
        (true, "centralized", runtimeNs)
    )

    println("replicas,num_deltas_total,topology,enforcing,runtime_ns")
    (resultsP2p ++ resultsCentralized)
      .foreach((enforcementEnabled, topology, runtimeNs) =>
        println(s"$NUM_REPLICAS,$NUM_DELTAS_TOTAL,$topology,$enforcementEnabled,$runtimeNs")
      )

  }

  def benchmarkP2p(enforcementEnabled: Boolean, trace: Trace): Long = {
    val endStateReachedLatch = CountDownLatch(NUM_REPLICAS)

    val replicas = setupP2pSimulation(trace, enforcementEnabled, endStateReachedLatch)

    // Give the replicas some time to connect to each other and process all messages
    while replicas.exists(_.sync.connectedPeers.size != NUM_REPLICAS - 1)
    do Thread.sleep(100)
    Thread.sleep(100)

    // Make sure that all replicas are connected to all others
    require(replicas.forall(_.sync.connectedPeers.size == NUM_REPLICAS - 1))

    val runtimeNs = measureSimulationTimeToConvergence(endStateReachedLatch, replicas, trace.deltas)

    replicas.foreach(_.stop())

    runtimeNs
  }

  def benchmarkCentralized(trace: Trace): Long = {
    val endStateReachedLatch = CountDownLatch(NUM_REPLICAS)

    val (replicas, relay) = setupCentralizedSimulation(trace, endStateReachedLatch)

    // Give the replicas some time to connect to each other and process all messages
    while relay.sync.connectedPeers.size != NUM_REPLICAS
    do Thread.sleep(100)
    Thread.sleep(100)

    // Make sure that all replicas are only connected to relay and relay is connected to all others
    require(replicas.forall(_.sync.connectedPeers.size == 1))
    // Ensure that ACL version is the same on all replicas / relay
    require(relay.sync.connectedPeers.size == NUM_REPLICAS)
    require(relay.sync.aclVersion == replicas(0).sync.aclVersion)
    require(replicas.map(_.sync.aclVersion).toSet.size == 1)
    require(relay.sync.currentAcl.read(relay.identity.getPublic) == PermissionTree.allow)

    val runtimeNs = measureSimulationTimeToConvergence(endStateReachedLatch, replicas, trace.deltas)

    replicas.foreach(_.stop())
    relay.stop()

    runtimeNs
  }

  def measureSimulationTimeToConvergence(
      stopLatch: CountDownLatch,
      replicas: Array[BenchmarkReplica],
      deltas: Array[Array[TravelPlan]]
  ): Long = {
    // Wait for all replicas + this thread
    val startLatch = CountDownLatch(NUM_REPLICAS + 1)

    // Start benchmark (waits until all are ready)
    Executors.newVirtualThreadPerTaskExecutor()
    replicas.zipWithIndex
      .foreach((replica, idx) =>
        Thread.startVirtualThread(() =>
            startLatch.countDown() // Signal ready to start
            startLatch.await()     // Wait for all other threads before starting
            replica.applyAllMutations(deltas(idx))
        )
      )

    // Wait until all threads are ready
    startLatch.countDown()
    startLatch.await()

    val startTime = System.nanoTime()
    stopLatch.await()
    val endTime = System.nanoTime()

    endTime - startTime
  }

  private def setupP2pSimulation(
      trace: Trace,
      enforcementEnabled: Boolean,
      finishedLatch: CountDownLatch
  ): Array[BenchmarkReplica] = {
    val endStateVersion = trace.computeEndStateVersion(enforcementEnabled)

    val replicas = trace.ids.map(id =>
      BenchmarkReplica(
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

  private def setupCentralizedSimulation(
      trace: Trace,
      finishedLatch: CountDownLatch
  ): (Array[BenchmarkReplica], BenchmarkRelayReplica) = {
    val endStateVersion = trace.computeEndStateVersion(true)
    val relayIdentity   = IdentityFactory.createNewIdentity
    val relay           = BenchmarkRelayReplica("localhost", relayIdentity, trace.genesis)
    relay.start()

    val replicas = trace.ids.map(id =>
      BenchmarkReplica(
        "localhost",
        id,
        trace.genesis,
        enforcing = true,
        replica =>
          if endStateVersion == replica.sync.stateVersion
          then finishedLatch.countDown()
      )
    )

    // Assign permissions
    replicas(0).sync.delegatePermission(trace.additionalPermissions)
    // Relay needs full read permission
    replicas(0).sync.delegatePermission(read = Map(relay.identity.getPublic -> PermissionTree.allow))

    // Connect to relay
    replicas.foreach(_.start())
    replicas.foreach { replica =>
      replica.sync.connect(relayIdentity.getPublic, "localhost", relay.sync.listenAddress.get._2)
    }

    (replicas, relay)
  }
}
