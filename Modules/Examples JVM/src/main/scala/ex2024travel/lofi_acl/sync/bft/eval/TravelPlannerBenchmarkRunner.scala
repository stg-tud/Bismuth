package ex2024travel.lofi_acl.sync.bft.eval

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromStream, writeToStream}
import ex2024travel.lofi_acl.sync.bft.BftAclOpGraph.Signature
import ex2024travel.lofi_acl.sync.bft.eval.TravelPlannerBenchmark.createTrace
import ex2024travel.lofi_acl.sync.bft.{ReplicaWithBftAcl, SerializedAclOp}
import ex2024travel.lofi_acl.travelplanner.TravelPlan
import rdts.filters.PermissionTree

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.collection.mutable

object TravelPlannerBenchmarkRunner {

  def main(args: Array[String]): Unit = {
    val traceFile = Paths.get("./results/lofi_acl/trace-100_000.json")
    // TraceGen.run(traceFile, 100_000)
    TraceReplay.run(traceFile, Map(0 -> Set(1, 2, 3)))
  }

  def permissionAssignmentFull(bench: TravelPlannerBenchmark): Unit = {
    bench.assignPermission(0, 1, PermissionTree.allow, PermissionTree.allow)
    bench.assignPermission(0, 2, PermissionTree.allow, PermissionTree.allow)
    bench.assignPermission(0, 3, PermissionTree.allow, PermissionTree.allow)
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
      val next    = stack.pop()
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
    def run(traceFile: Path, connectionMap: Map[Int, Set[Int]], numOps: Int): Unit = {
      // Run
      val start                         = System.nanoTime()
      val (trace, notifications, bench) = createTrace(4, numOps, connectionMap, permissionAssignmentPartial)
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
      val savedTrace =
        SavedTrace(bench.identities, bench.aclRoot, bench.replicas(0).currentOpGraph.ops, trace, notifications)
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
    def run(traceFile: Path, connectionMap: Map[Int, Set[Int]]): Unit = {
      // Prepare
      val restoredTrace = readFromStream[SavedTrace](Files.newInputStream(traceFile, StandardOpenOption.READ))
      val bench         = TravelPlannerBenchmark(
        restoredTrace.identities.length,
        restoredTrace.identities,
        restoredTrace.aclRoot
      )
      val aclOps = restoredTrace.aclOps.map((sig, op) => sig -> op.serialize(sig))
      bench.replicas.foreach(applyAllAclOps(_, aclOps))
      bench.connect(connectionMap)

      // Run
      val start = System.nanoTime()
      bench.replayTrace(restoredTrace.deltaTrace, restoredTrace.notificationTrace)
      val stop = System.nanoTime()
      println(s"${(stop - start) / 1_000_000}ms")
    }
  }

  object Scenarios {
    def fullMesh(bench: TravelPlannerBenchmark): Unit = {}
  }
}
