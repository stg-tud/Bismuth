package lofi_acl.evaluation

import crypto.channels.IdentityFactory
import lofi_acl.travelplanner.TravelPlan
import rdts.filters.PermissionTree

import java.util.concurrent.{CountDownLatch, Executors}
import scala.util.Random

object SimulationBenchmark {
  private val SEED                            = 42
  private val MIN_ENTRIES_PER_MAP_PER_REPLICA = 10
  private val MAX_ENTRIES_PER_MAP_PER_REPLICA = 100

  def main(args: Array[String]): Unit = {
    val numReplicas = 10
    val numDeltas   = 10_000 * numReplicas

    println("Performing 10 warmup runs")
    start(numDeltasTotal = 100_000, numReplicas = 10, numRepetitions = 10, None): Unit
    println("Warmup complete")

    val numReplicaParameters          = List(10)
    val numDeltasPerReplicaParameters = List(10_000, 100_000)
    val numRepetitions: Int => Int    = numDeltasPerReplica => if numDeltasPerReplica <= 100 then 200 else 20

    var results: List[String] = numReplicaParameters.flatMap { numReplicas =>
      numDeltasPerReplicaParameters.flatMap { numDeltasPerReplica =>
        val numDeltasTotal = numDeltasPerReplica * numReplicas
        start(numDeltasTotal, numReplicas, numRepetitions(numDeltasPerReplica), delayMillis = None) // Some(0) != None
      }
    }

    println("Performing 10 warmup runs")
    start(numDeltasTotal = 10, numReplicas = 10, numRepetitions = 10, Some(0)): Unit
    println("Warmup complete")

    results ++= List(0, 1, 2, 4, 8, 16, 32, 64).flatMap { delayMillis =>
      List(10).flatMap { numReplicas =>
        List(1).flatMap { numDeltasPerReplica =>
          val numDeltasTotal = numDeltasPerReplica * numReplicas
          start(numDeltasTotal, numReplicas, 200, Some(delayMillis))
        }
      }
    }

    println("replicas,num_deltas_total,delay_ms,centralized,enforcing,runtime_ns")
    println(results.mkString("\n"))
  }

  def start(numDeltasTotal: Int, numReplicas: Int, numRepetitions: Int, delayMillis: Option[Int]): Seq[String] = {
    require(numDeltasTotal / numReplicas * numReplicas == numDeltasTotal)
    given Random     = Random(SEED)
    val trace: Trace = TraceGeneration.generateTrace(
      numReplicas,
      numDeltasTotal / numReplicas,
      MIN_ENTRIES_PER_MAP_PER_REPLICA,
      MAX_ENTRIES_PER_MAP_PER_REPLICA
    )

    val results = (0 until numRepetitions * 4).map(i =>
        val enforceAcl  = (i & 1) == 0
        val centralized = (i & 2) == 2
        val runtimeNs   = benchmark(enforceAcl, centralized, trace, delayMillis)
        println(
          s"(numDeltas=$numDeltasTotal,numReplicas=$numReplicas): [${i + 1}/${numRepetitions * 4}] centralized=$centralized, enforcement=$enforceAcl, runtime_ms=${runtimeNs / 1_000_000}"
        )

        (enforceAcl, centralized, runtimeNs)
    )

    // "replicas,num_deltas_total,delay_ms,centralized,enforcing,runtime_ns"
    results.map((enforcementEnabled, centralized, runtimeNs) =>
      s"$numReplicas,$numDeltasTotal,${delayMillis.orNull},$centralized,$enforcementEnabled,$runtimeNs"
    )
  }

  def benchmark(enforceAcl: Boolean, centralized: Boolean, trace: Trace, delayMillis: Option[Int]): Long = {
    if centralized then benchmarkCentralized(enforceAcl, trace, delayMillis)
    else benchmarkP2p(enforceAcl, trace, delayMillis)
  }

  def benchmarkP2p(enforceAcl: Boolean, trace: Trace, delayMillis: Option[Int]): Long = {
    val endStateReachedLatch = CountDownLatch(trace.ids.length)

    val replicas = setupP2pSimulation(trace, enforceAcl, endStateReachedLatch, delayMillis)

    // Give the replicas some time to connect to each other and process all messages
    while replicas.exists(_.sync.connectedPeers.size != replicas.length - 1)
    do Thread.sleep(100)
    Thread.sleep(100)

    // Make sure that all replicas are connected to all others
    require(replicas.forall(_.sync.connectedPeers.size == replicas.length - 1))

    val runtimeNs = measureSimulationTimeToConvergence(endStateReachedLatch, replicas, trace.deltas, replicas.length)

    replicas.foreach(_.stop())

    runtimeNs
  }

  def benchmarkCentralized(enforceAcl: Boolean, trace: Trace, delayMillis: Option[Int]): Long = {
    val endStateReachedLatch = CountDownLatch(trace.ids.length)

    val (replicas, relay) = setupCentralizedSimulation(trace, endStateReachedLatch, enforceAcl, delayMillis)

    // Give the replicas some time to connect to each other and process all messages
    while relay.sync.connectedPeers.size != trace.ids.length
    do Thread.sleep(100)
    Thread.sleep(100)

    // Make sure that all replicas are only connected to relay and relay is connected to all others
    require(replicas.forall(_.sync.connectedPeers.size == 1))

    if enforceAcl then
        // Ensure that ACL version is the same on all replicas / relay
        require(relay.sync.connectedPeers.size == trace.ids.length)
        require(replicas.forall(_.sync.aclVersion == relay.sync.aclVersion))
        require(replicas.map(_.sync.aclVersion).toSet.size == 1)
        require(relay.sync.currentAcl.read(relay.identity.getPublic) == PermissionTree.allow)

    val runtimeNs = measureSimulationTimeToConvergence(endStateReachedLatch, replicas, trace.deltas, trace.ids.length)

    replicas.foreach(_.stop())
    relay.stop()

    runtimeNs
  }

  def measureSimulationTimeToConvergence(
      stopLatch: CountDownLatch,
      replicas: Array[BenchmarkReplica],
      deltas: Array[Array[TravelPlan]],
      numReplicas: Int
  ): Long = {
    // Wait for all replicas + this thread
    val startLatch = CountDownLatch(numReplicas + 1)

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
      finishedLatch: CountDownLatch,
      delayMillis: Option[Int]
  ): Array[BenchmarkReplica] = {
    val endStateVersion = trace.computeEndStateVersion

    val replicas = trace.ids.map(id =>
      BenchmarkReplica(
        "localhost",
        id,
        trace.genesis,
        enforcementEnabled,
        replica =>
          if endStateVersion == replica.sync.stateVersion
          then finishedLatch.countDown(),
        delayMillis
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
      finishedLatch: CountDownLatch,
      enforceAcl: Boolean,
      delayMillis: Option[Int]
  ): (Array[BenchmarkReplica], BenchmarkRelayReplica) = {
    val endStateVersion = trace.computeEndStateVersion
    val relayIdentity   = IdentityFactory.createNewIdentity
    val relay           = BenchmarkRelayReplica("localhost", relayIdentity, trace.genesis, enforceAcl, delayMillis)
    relay.start()

    val replicas = trace.ids.map(id =>
      BenchmarkReplica(
        "localhost",
        id,
        trace.genesis,
        enforceAcl,
        replica =>
          if endStateVersion == replica.sync.stateVersion
          then finishedLatch.countDown(),
        delayMillis
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
