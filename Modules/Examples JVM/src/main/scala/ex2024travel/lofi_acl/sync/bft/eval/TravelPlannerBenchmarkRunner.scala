package ex2024travel.lofi_acl.sync.bft.eval

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromStream, writeToStream}
import ex2024travel.lofi_acl.sync.bft.BftAclOpGraph.Signature
import ex2024travel.lofi_acl.sync.bft.eval.SavedTrace.NotificationTrace
import ex2024travel.lofi_acl.sync.bft.eval.TravelPlannerBenchmark.createTrace
import ex2024travel.lofi_acl.sync.bft.{ReplicaWithBftAcl, SerializedAclOp}
import rdts.filters.PermissionTree

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.collection.mutable
import scala.util.Random

object TravelPlannerBenchmarkRunner {

  def main(args: Array[String]): Unit = {
    val numReplicas = 4
    val numOps      = 50_000

    // val traceFile   = Paths.get(s"./results/lofi_acl/trace-$numOps.json")
    // TraceGen.run(traceFile, permissionAssignmentPartial, centralServerConnectionMap(numReplicas), numOps)

    val traceFile = Paths.get(s"./results/lofi_acl/trace-valley-$numOps.json")
    //TraceGen.run(traceFile, permissionValleyPermissionAssignment, permissionValleyConnectionMap, numOps)
    val trace = TraceReplay.readTrace(traceFile)

    TraceReplay.run(trace): Unit
    //// Without Filtering (ACL off)
    //TraceReplay.run(trace, withAcl = false)

    //// Test same trace with full mesh instead of central relay
    //val fullMeshConnMap = fullMeshConnectionMap(numReplicas)
    //val modifiedTrace   = trace.copy(
    //  connectionMap = fullMeshConnMap,
    //  notificationTrace = TraceReplay.generateNotificationTrace(fullMeshConnMap, trace.deltaTrace.length, numReplicas)
    //)
    //TraceReplay.run(modifiedTrace)                  // With ACL
    //TraceReplay.run(modifiedTrace, withAcl = false) // Without

    //// Test trace with permission valley
    //val permissionValleyTrace = trace.copy(
    //  connectionMap = permissionValleyConnectionMap,
    //  notificationTrace =
    //    TraceReplay.generateNotificationTrace(permissionValleyConnectionMap, trace.deltaTrace.length, numReplicas)
    //)
    //TraceReplay.run(permissionValleyTrace)
    //()
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
    // bench.assignPermission(0, 1, PermissionTree.fromPath("title"), PermissionTree.fromPath("title"))
    // bench.assignPermission(0, 2, PermissionTree.fromPath("title"), PermissionTree.fromPath("title"))
    // bench.assignPermission(0, 2, PermissionTree.fromPath("expenses"), PermissionTree.fromPath("expenses"))
    // bench.assignPermission(0, 3, PermissionTree.fromPath("title"), PermissionTree.fromPath("title"))
    // bench.assignPermission(0, 3, PermissionTree.fromPath("bucketList"), PermissionTree.fromPath("bucketList"))

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
        numOps: Int
    ): Unit = {
      // Run
      val start                         = System.nanoTime()
      val (trace, notifications, bench) = createTrace(4, numOps, connectionMap, permissionAssignment)
      val stop                          = System.nanoTime()
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
      notifications.mapInPlace(_.mapInPlace(i =>
        neighbours(i).drop(random.nextInt(neighbours(i).size)).head
      ))
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
      println(s"${(times.last - times.head) / 1_000_000}ms: ${timePerThousand.map(_ / 1_000_000).mkString(",")}")
      timePerThousand
    }
  }

  object Scenarios {
    def fullMesh(bench: TravelPlannerBenchmark): Unit = {}
  }
}
