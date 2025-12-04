package lofi_acl.sync.bft.eval

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromStream, writeToStream}
import lofi_acl.sync.bft.BftAclOpGraph.Signature
import SavedTrace.NotificationTrace
import TravelPlannerBenchmark.createTrace
import lofi_acl.sync.bft.{ReplicaWithBftAcl, SerializedAclOp}
import rdts.filters.PermissionTree

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.collection.mutable
import scala.util.Random

object TravelPlannerBenchmarkRunner {

  def main(args: Array[String]): Unit = {
    val numReplicas = 4
    val numOps      = 100_000

    val traceFile = Paths.get(s"./results/lofi_acl/valley-$numReplicas-$numOps-trace.json")
    // TraceGen.run(traceFile, permissionValleyPermissionAssignment, permissionValleyConnectionMap, numOps, 140, 160)

    val valleyTrace = TraceReplay.readTrace(traceFile)

    val valleyTraceButFullMesh = valleyTrace.copy(
      connectionMap = fullMeshConnectionMap(numReplicas),
      notificationTrace = TraceReplay.generateNotificationTrace(
        fullMeshConnectionMap(numReplicas),
        valleyTrace.deltaTrace.length,
        numReplicas
      )
    )

    val valleyTraceButCentralized = valleyTrace.copy(
      connectionMap = centralServerConnectionMap(numReplicas),
      notificationTrace = TraceReplay.generateNotificationTrace(
        centralServerConnectionMap(numReplicas),
        valleyTrace.deltaTrace.length,
        numReplicas
      )
    )

    // Warmup
    TraceReplay.run(valleyTrace): Unit

    val results = List(
      ("valley", "acl", TraceReplay.run(valleyTrace, withAcl = true)),                        // Valley with ACL,
      ("valley", "no-acl", TraceReplay.run(valleyTrace, withAcl = false)),                    // Valley without ACL,
      ("full-mesh", "acl", TraceReplay.run(valleyTraceButFullMesh, withAcl = true)),          // Full mesh with ACL,
      ("full-mesh", "no-acl", TraceReplay.run(valleyTraceButFullMesh, withAcl = false)),      // Full mesh without ACL,
      ("centralized", "acl", TraceReplay.run(valleyTraceButCentralized, withAcl = true)),     // Centralized with ACL
      ("centralized", "no-acl", TraceReplay.run(valleyTraceButCentralized, withAcl = false)), // Centralized without ACL
    )

    val resultsFile = Paths.get(s"./results/lofi_acl/valley-$numReplicas-$numOps-results.csv")
    saveResultsCsv(results, resultsFile)
  }

  def saveResultsCsv(results: Seq[(String, String, Array[Long])], resultsFile: Path): Unit = {
    val resultOutStream = PrintWriter(Files.newOutputStream(
      resultsFile,
      StandardOpenOption.WRITE,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    ))

    resultOutStream.println("topology,acl,num_ops_before,time_delta_ns") // header
    results.foreach { (topology, aclStatus, times) =>
      times.indices.foreach { timeIndex =>
        resultOutStream.println(s"$topology,$aclStatus,${timeIndex * 1_000},${times(timeIndex)}")
      }
    }

    resultOutStream.close()
  }

  def fullMeshConnectionMap(numReplicas: Int): Map[Int, Set[Int]] =
    (0 until (numReplicas - 1)).map(i => i -> ((i + 1) until numReplicas).toSet).toMap

  def centralServerConnectionMap(numReplicas: Int): Map[Int, Set[Int]] =
    Map(0 -> (1 until numReplicas).toSet)

  // ┌ 1 ┐
  // 0 │ 3
  // └ 2 ┘
  val permissionValleyConnectionMap: Map[Int, Set[Int]] =
    Map(
      0 -> Set(1, 2),
      1 -> Set(2, 3),
      2 -> Set(3)
    )
  def permissionValleyPermissionAssignment(bench: TravelPlannerBenchmark): Unit = {
    require(bench.numReplicas == 4)
    bench.assignPermission(0, 1, PermissionTree.fromPath("title"), PermissionTree.fromPath("title"))
    bench.assignPermission(0, 1, PermissionTree.fromPath("expenses"), PermissionTree.fromPath("expenses"))
    bench.assignPermission(0, 2, PermissionTree.fromPath("title"), PermissionTree.fromPath("title"))
    bench.assignPermission(0, 2, PermissionTree.fromPath("bucketList"), PermissionTree.fromPath("bucketList"))
    bench.assignPermission(0, 3, PermissionTree.fromPath("*"), PermissionTree.fromPath("expenses"))
  }

  def permissionAssignmentFull(bench: TravelPlannerBenchmark): Unit = {
    (1 until bench.numReplicas).foreach { receivingReplicas =>
      bench.assignPermission(0, receivingReplicas, PermissionTree.allow, PermissionTree.allow)
    }
  }

  def permissionAssignmentPartial(bench: TravelPlannerBenchmark): Unit = {
    require(bench.replicas.length == 4)
    bench.assignPermission(0, 1, PermissionTree.empty, PermissionTree.fromPath("title"))
    bench.assignPermission(
      0,
      2,
      PermissionTree.fromPath("title").merge(PermissionTree.fromPath("bucketList")),
      PermissionTree.fromPath("expenses")
    )
    bench.assignPermission(0, 3, PermissionTree.fromPath("title"), PermissionTree.fromPath("bucketList"))

    bench.antiEntropyInstances.foreach(_.processAllMessagesInInbox(0))
  }

  def applyAllAclOps(replica: ReplicaWithBftAcl[?], aclOps: Map[Signature, SerializedAclOp]): Unit = {
    var remaining = aclOps
    val stack     = mutable.Stack.empty[Signature]
    while remaining.nonEmpty do {
      stack.addOne(remaining.head._1)
      val next = stack.pop()
      if remaining.contains(next) then
          val missing = replica.applyAclOpIfPossible(remaining(next))
          if missing.nonEmpty then {
            stack.push(next)
            stack.pushAll(missing)
          } else {
            remaining = remaining.removed(next)
          }
    }
  }

  object TraceGen {
    def run(
        traceFile: Path,
        permissionAssignment: TravelPlannerBenchmark => Unit,
        connectionMap: Map[Int, Set[Int]],
        numOps: Int,
        mapMinEntries: Int,
        mapMaxEntries: Int
    ): Unit = {
      // Run
      val start                         = System.nanoTime()
      val (trace, notifications, bench) =
        createTrace(4, numOps, connectionMap, permissionAssignment, mapMinEntries, mapMaxEntries)
      val stop = System.nanoTime()
      println(s"${(stop - start) / 1_000_000}ms")

      // Save
      Files.createDirectories(traceFile.getParent)
      val traceOutputStream = Files.newOutputStream(
        traceFile,
        StandardOpenOption.WRITE,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
      val savedTrace = SavedTrace(
        numOps,
        bench.identities,
        bench.aclRoot,
        bench.replicas(0).currentOpGraph.ops,
        connectionMap,
        trace,
        notifications
      )
      writeToStream(savedTrace, traceOutputStream)
      traceOutputStream.close()

      bench.replicas.zipWithIndex.foreach { (replica, idx) =>
        val path = Paths.get(s"./results/lofi_acl/replica-$idx")
        val out  = Files.newOutputStream(
          path,
          StandardOpenOption.WRITE,
          StandardOpenOption.CREATE,
          StandardOpenOption.TRUNCATE_EXISTING
        )
        writeToStream(replica.currentState, out)
        out.close()
      }
    }
  }

  object TraceReplay {
    def readTrace(traceFile: Path): SavedTrace =
      readFromStream[SavedTrace](Files.newInputStream(traceFile, StandardOpenOption.READ))

    def generateNotificationTrace(
        connectionMap: Map[Int, Set[Int]],
        numRounds: Int,
        numReplicas: Int
    ): NotificationTrace = {
      val neighbours = Array.fill[Set[Int]](numReplicas)(Set.empty)
      connectionMap.foreach((from, to) =>
          neighbours(from) = neighbours(from) ++ to
          to.foreach(r => neighbours(r) = neighbours(r) + from)
      )
      val peerCount     = neighbours.map(_.size)
      val random        = Random(42)
      val notifications = Array.ofDim[Int](numRounds, numReplicas)
      notifications.foreach(notificationsInRound =>
        notificationsInRound.indices.foreach(i =>
          notificationsInRound(i) = neighbours(i).drop(random.nextInt(peerCount(i))).head
        )
      )
      notifications
    }

    def run(trace: SavedTrace, withAcl: Boolean = true): Array[Long] = {
      // Prepare
      val bench = TravelPlannerBenchmark(
        trace.identities.length,
        trace.identities,
        trace.aclRoot,
        withAcl
      )
      val aclOps = trace.aclOps.map((sig, op) => sig -> op.serialize(sig))
      bench.replicas.foreach(applyAllAclOps(_, aclOps))
      bench.connect(trace.connectionMap)

      // Run
      val times           = bench.replayTrace(trace.deltaTrace, trace.notificationTrace, trace.numOps)
      val timePerThousand = times.take(times.length - 1).zip(times.drop(1)).map((prev, cur) => cur - prev)
      // println(s"${(times.last - times.head) / 1_000_000}ms: ${timePerThousand.map(_ / 1_000_000).mkString(",")}")
      println(s"${(times.last - times.head) / 1_000_000}ms")
      timePerThousand
    }
  }

  object Scenarios {
    def fullMesh(bench: TravelPlannerBenchmark): Unit = {}
  }
}
