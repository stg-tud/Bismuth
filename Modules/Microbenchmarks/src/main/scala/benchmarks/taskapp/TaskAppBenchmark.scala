package benchmarks.taskapp

import benchmarks.taskapp.TaskApp.{App, Entry, given}
import rdts.datatypes.ReplicatedTree
import rdts.base.LocalUid
import rdts.experiments.UndoRedoReplica
import rdts.experiments.DeltaHistory
import org.openjdk.jol.info.GraphLayout

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import rdts.datatypes.LastWriterWins

object TaskAppBenchmark {

  val numInteractionsPerReplica = 1_000_000
  val checkpointEvery           = 1_000
  val progressLogEvery          = 1_000

  // Memory tracking (uses JOL GraphLayout; enable in Settings.jolSettings)
  val recordMemory      = true
  val memorySampleEvery = 1_000 // 1 = every interaction; increase to reduce overhead

  // Two replicas with separate LocalUids
  val localUid1: LocalUid = LocalUid.predefined("benchmark-client-1")
  val localUid2: LocalUid = LocalUid.predefined("benchmark-client-2")

  var app1: App = scala.compiletime.uninitialized
  var app2: App = scala.compiletime.uninitialized

  // Track which replica is active (alternates between 1 and 2)
  var activeReplica: Int = 1

  // Timing trackers per operation type
  case class OpStats(
      var count: Long = 0,
      var totalNanos: Long = 0,
      samples: mutable.ArrayBuffer[Long] = mutable.ArrayBuffer.empty
  ) {
    def avgMs: Double = if count == 0 then 0.0 else totalNanos / (count * 1_000_000.0)

    def p90Ms: Double = percentileMs(0.90)
    def p99Ms: Double = percentileMs(0.99)

    def record(nanos: Long): Unit = {
      count += 1
      totalNanos += nanos
      samples += nanos
    }

    private def percentileMs(p: Double): Double = {
      if samples.isEmpty then 0.0
      else {
        val sorted  = samples.sorted
        val idx     = math.ceil(p * sorted.size).toInt - 1
        val clamped = math.max(0, math.min(idx, sorted.size - 1))
        sorted(clamped) / 1_000_000.0
      }
    }

    def reset(): Unit = {
      count = 0
      totalNanos = 0
      samples.clear()
    }
  }

  // Operation categories for measurement
  val undoOps = Set("Undo")
  val treeOps = Set(
    "AddTaskList",
    "AddFolder",
    "MoveEntry",
    "RemoveEntry",
    "UpdateFolderName",
    "UpdateTaskListName"
  )
  val listOps = Set(
    "AddTask",
    "RemoveTaskListItem",
    "MoveTaskListItem",
    "UpdateTaskTitle",
    "UpdateTaskDone",
    "UpdateTaskDescription",
    "MarkItemsAsDoneThatMatch"
  )

  val opTimings: mutable.Map[String, OpStats] = mutable.Map(
    "AddFolder"                -> OpStats(),
    "AddTaskList"              -> OpStats(),
    "MoveEntry"                -> OpStats(),
    "RemoveEntry"              -> OpStats(),
    "UpdateFolderName"         -> OpStats(),
    "UpdateTaskListName"       -> OpStats(),
    "AddTask"                  -> OpStats(),
    "RemoveTaskListItem"       -> OpStats(),
    "MoveTaskListItem"         -> OpStats(),
    "UpdateTaskTitle"          -> OpStats(),
    "UpdateTaskDescription"    -> OpStats(),
    "UpdateTaskDone"           -> OpStats(),
    "MarkItemsAsDoneThatMatch" -> OpStats(),
    "Undo"                     -> OpStats(),
    "Redo"                     -> OpStats(),
    "read"                     -> OpStats(), // Track app.read calls separately
    "Sync"                     -> OpStats(), // Track sync between replicas
  )

  private val interactionSamplesNanos: mutable.ArrayBuffer[Long]    = mutable.ArrayBuffer.empty
  private val opLatencySamples: mutable.ArrayBuffer[(String, Long)] = mutable.ArrayBuffer.empty
  private val opPercentileHistory: mutable.ArrayBuffer[(Long, String, Double, String, Double)] =
    mutable.ArrayBuffer.empty

  def timed[T](opName: String)(op: => T): T = {
    val start   = System.nanoTime()
    val result  = op
    val elapsed = System.nanoTime() - start
    opTimings(opName).record(elapsed)
    opLatencySamples += ((opName, elapsed))
    result
  }

  // Helper to get the current active app
  inline def currentApp: App      = if activeReplica == 1 then app1 else app2
  inline def currentUid: LocalUid = if activeReplica == 1 then localUid1 else localUid2

  inline def timedRead: ReplicatedTree[Entry] = timed("read")(currentApp.read)

  // Track sync time separately to exclude from interaction timing
  var syncTimeInCurrentBatch: Long = 0

  def main(args: Array[String]): Unit = {
    println("Warm Up...")

    // Warm-up: run a small number of interactions to trigger JIT and class loading
    app1 = App(UndoRedoReplica.empty[ReplicatedTree[Entry]])
    app2 = App(UndoRedoReplica.empty[ReplicatedTree[Entry]])
    val warmupGenerator = new TaskAppInteractionGenerator()

    activeReplica = 1
    val warmDelta1 = performInteraction(warmupGenerator.initialInteraction())
    app2.applyDelta(warmDelta1)
    activeReplica = 2
    val warmDelta2 = performInteraction(warmupGenerator.initialInteraction())
    app1.applyDelta(warmDelta2)

    val warmupRounds = 2_000
    var i            = 0
    while i < warmupRounds do {
      val interaction1 = warmupGenerator.nextInteraction(app1.read)
      val interaction2 = warmupGenerator.nextInteraction(app2.read)

      activeReplica = 1
      val delta1 = performInteraction(interaction1)
      activeReplica = 2
      val delta2 = performInteraction(interaction2)
      app2.applyDelta(delta1)
      app1.applyDelta(delta2)

      i += 1
    }
    opTimings.values.foreach(_.reset())
    rdts.experiments.ReplicaTimings.reset()

    println("Starting TaskApp benchmark with two replicas...")

    // Initialize both apps with empty Replicas
    app1 = App(UndoRedoReplica.empty[ReplicatedTree[Entry]])
    app2 = App(UndoRedoReplica.empty[ReplicatedTree[Entry]])

    // Create the live interaction generator
    val generator = new TaskAppInteractionGenerator()

    // Setup CSV output
    val csvFilePath: Path = Paths.get("./benchmarks/results/taskapp_benchmark.csv")
    Files.createDirectories(csvFilePath.getParent)
    val csvFile = new PrintWriter(csvFilePath.toFile)

    csvFile.println(
      "interactions,treeSize,folderCount,taskListCount,totalTaskCount,avgInteractionMs,p90InteractionMs,p99InteractionMs,avgUndoMs,avgTreeMs,avgListMs,concurrentMoveTreeConflicts,concurrentMoveListConflicts"
    )
    csvFile.flush()

    val memoryCsvFilePath: Path            = Paths.get("./benchmarks/results/taskapp_replica_memory.csv")
    val memoryCsvFile: Option[PrintWriter] = if recordMemory then {
      Files.createDirectories(memoryCsvFilePath.getParent)
      val f = new PrintWriter(memoryCsvFilePath.toFile)
      f.println("interactions,phase,app1Bytes,app2Bytes")
      f.flush()
      Some(f)
    } else None

    val startNanoTime: Long             = System.nanoTime()
    var lastCheckPointEndNanoTime: Long = startNanoTime

    var counter                     = 0
    var concurrentMoveTreeConflicts = 0
    var concurrentMoveListConflicts = 0

    println("Running benchmark interactions (1,000,000 per replica)...")

    activeReplica = 1
    val initDelta1 = performInteraction(generator.initialInteraction())
    app2.applyDelta(initDelta1)
    activeReplica = 2
    val initDelta2 = performInteraction(generator.initialInteraction())
    app1.applyDelta(initDelta2)
    counter += 1

    var avgTime              = 0L
    var lastAvgInteractionMs = 0.0
    var lastP90InteractionMs = 0.0
    var lastP99InteractionMs = 0.0
    var lastP95Op: String    = ""
    var lastP99Op: String    = ""
    var lastP95OpMs          = 0.0
    var lastP99OpMs          = 0.0
    var lastTreeSize         = 0
    var lastFolderCount      = 0
    var lastTaskListCount    = 0
    var lastTotalTaskCount   = 0
    while counter < numInteractionsPerReplica do {
      val interaction1 = generator.nextInteraction(app1.read)
      val interaction2 = generator.nextInteraction(app2.read)

      (interaction1, interaction2) match
          case (MoveEntry(entryId1, _), MoveEntry(entryId2, _)) if entryId1 == entryId2 =>
            concurrentMoveTreeConflicts += 1
          case (MoveTaskListItem(taskListId1, _, _), MoveTaskListItem(taskListId2, _, _))
              if taskListId1 == taskListId2 =>
            concurrentMoveListConflicts += 1
          case _ => ()

      var syncStartTime = System.nanoTime()
      activeReplica = 1
      val delta1 = performInteraction(interaction1)
      activeReplica = 2
      val delta2 = performInteraction(interaction2)
      app2.applyDelta(delta1)
      app1.applyDelta(delta2)
      val roundDuration = System.nanoTime() - syncStartTime
      avgTime += roundDuration
      interactionSamplesNanos += (roundDuration / 2)

      if recordMemory && counter % memorySampleEvery == 0 then
          val app1Bytes = replicaSizeBytes(app1)
          val app2Bytes = replicaSizeBytes(app2)
          memoryCsvFile.foreach { f =>
            f.println(s"$counter,after-sync,$app1Bytes,$app2Bytes")
            f.flush()
          }

      counter += 1

      if counter % checkpointEvery == 0 then {
        lastAvgInteractionMs = avgTime / checkpointEvery / 2 / 1_000_000.0
        lastP90InteractionMs = percentileMs(interactionSamplesNanos, 0.90)
        lastP99InteractionMs = percentileMs(interactionSamplesNanos, 0.99)
        val p95Op = percentileOp(opLatencySamples, 0.95)
        val p99Op = percentileOp(opLatencySamples, 0.99)
        lastP95Op = p95Op._1
        lastP95OpMs = p95Op._2 / 1_000_000.0
        lastP99Op = p99Op._1
        lastP99OpMs = p99Op._2 / 1_000_000.0
        if lastP95Op.nonEmpty && lastP99Op.nonEmpty then
            opPercentileHistory += ((counter, lastP95Op, lastP95OpMs, lastP99Op, lastP99OpMs))
        avgTime = 0L
        interactionSamplesNanos.clear()
        opLatencySamples.clear()
        val tree = app1.read
        lastTreeSize = tree.size
        val (folderCount, taskListCount, totalTaskCount) = countEntries(tree)
        lastFolderCount = folderCount
        lastTaskListCount = taskListCount
        lastTotalTaskCount = totalTaskCount

        // Compute Undo/Tree/List avg times for this checkpoint
        def avgCat(cat: Set[String]): Double = {
          val stats      = cat.flatMap(opTimings.get)
          val totalNanos = stats.map(_.totalNanos).sum
          val count      = stats.map(_.count).sum
          if count == 0 then 0.0 else totalNanos / (count * 1_000_000.0)
        }
        val avgUndoMs = avgCat(undoOps)
        val avgTreeMs = avgCat(treeOps)
        val avgListMs = avgCat(listOps)

        csvFile.println(
          s"$counter,$lastTreeSize,$lastFolderCount,$lastTaskListCount,$lastTotalTaskCount,$lastAvgInteractionMs,$lastP90InteractionMs,$lastP99InteractionMs,$avgUndoMs,$avgTreeMs,$avgListMs,$concurrentMoveTreeConflicts,$concurrentMoveListConflicts"
        )
        csvFile.flush()

        lastCheckPointEndNanoTime = System.nanoTime()
      }

      if counter % progressLogEvery == 0 then {
        val historySize1 = app1.state.history.deltas.size
        val historySize2 = app2.state.history.deltas.size
        println(
          s"$counter/$numInteractionsPerReplica completed / avg: $lastAvgInteractionMs ms / p90: $lastP90InteractionMs ms / p99: $lastP99InteractionMs ms / tree: $lastTreeSize / history deltas: $historySize1/$historySize2"
        )
        if lastP95Op.nonEmpty && lastP99Op.nonEmpty then
            println(s"  P95 op: $lastP95Op (${lastP95OpMs} ms), P99 op: $lastP99Op (${lastP99OpMs} ms)")
        println(
          s"  Move tree conflicts: $concurrentMoveTreeConflicts, Move list conflicts: $concurrentMoveListConflicts"
        )
        // Print per-operation timing breakdown
        println("  Operation timings (last 1000):")
        opTimings.toSeq.sortBy(-_._2.avgMs).foreach { case (op, stats) =>
          if stats.count > 0 then
              // Find the longest operation name for alignment
              val maxOpLen = opTimings.keys.map(_.length).max
              val paddedOp = op.padTo(maxOpLen, ' ')
              println(
                f"    $paddedOp: ${stats.avgMs}%.4fms avg / p90 ${stats.p90Ms}%.4fms / p99 ${stats.p99Ms}%.4fms (${stats.count} calls, ${stats.totalNanos / 1_000_000.0}%.2fms total)"
              )
        }
        // Print Replica internal timing breakdown
        println(rdts.experiments.ReplicaTimings.report())
        rdts.experiments.ReplicaTimings.reset()
        // println(rdts.datatypes.ReplicatedTree.MergeTimings.report())
        // rdts.datatypes.ReplicatedTree.MergeTimings.reset()
        // Reset timings for next batch
        opTimings.values.foreach(_.reset())
      }
    }

    csvFile.close()
    memoryCsvFile.foreach(_.close())

    if opPercentileHistory.nonEmpty then
        val opPercentileFilePath: Path = Paths.get("./benchmarks/results/taskapp_op_percentiles.csv")
        Files.createDirectories(opPercentileFilePath.getParent)
        val opPercentileFile = new PrintWriter(opPercentileFilePath.toFile)
        opPercentileFile.println("interactions,p95Op,p95Ms,p99Op,p99Ms")
        opPercentileHistory.foreach { case (interactions, p95Op, p95Ms, p99Op, p99Ms) =>
          opPercentileFile.println(s"$interactions,$p95Op,$p95Ms,$p99Op,$p99Ms")
        }
        opPercentileFile.close()

    val totalTimeMs = (System.nanoTime() - startNanoTime) / 1_000_000.0
    println(s"Benchmark completed in ${totalTimeMs}ms")
    println(s"Total interactions: $counter")
    println(s"Results written to: ${csvFilePath.toAbsolutePath}")
  }

  private def replicaSizeBytes(app: App): Long =
    // Estimate per-replica heap usage by traversing the replica state object graph.
    GraphLayout.parseInstance(app.state).totalSize()

  private def percentileMs(samples: mutable.ArrayBuffer[Long], p: Double): Double = {
    if samples.isEmpty then 0.0
    else {
      val sorted  = samples.sorted
      val idx     = math.ceil(p * sorted.size).toInt - 1
      val clamped = math.max(0, math.min(idx, sorted.size - 1))
      sorted(clamped) / 1_000_000.0
    }
  }

  private def percentileOp(samples: mutable.ArrayBuffer[(String, Long)], p: Double): (String, Long) = {
    if samples.isEmpty then ("", 0L)
    else {
      val sorted  = samples.sortBy(_._2)
      val idx     = math.ceil(p * sorted.size).toInt - 1
      val clamped = math.max(0, math.min(idx, sorted.size - 1))
      sorted(clamped)
    }
  }

  private def countEntries(tree: ReplicatedTree[Entry]): (Int, Int, Int) = {
    var folderCount    = 0
    var taskListCount  = 0
    var totalTaskCount = 0

    tree.nodes.foreach { node =>
      node.value match {
        case Entry.FolderEntry(_) =>
          folderCount += 1
        case Entry.TaskListEntry(list) =>
          taskListCount += 1
          totalTaskCount += list.items.size
      }
    }

    (folderCount, taskListCount, totalTaskCount)
  }

  private def performInteraction(interaction: TaskAppInteraction): DeltaHistory[ReplicatedTree[Entry]] = {
    given LocalUid = currentUid
    val app        = currentApp

    val delta: DeltaHistory[ReplicatedTree[Entry]] = interaction match {
      case AddFolder(parentFolder, name) =>
        timed("AddFolder") { val l = app.addFolder(parentFolder, name); app.read; l }

      case AddTaskList(parentFolder, name) =>
        timed("AddTaskList") { val l = app.addTaskList(parentFolder, name); app.read; l }

      case MoveEntry(entryId, newParent) =>
        timed("MoveEntry") { val l = app.moveEntry(entryId, newParent); app.read; l }

      case RemoveEntry(entryId) =>
        timed("RemoveEntry") { val l = app.removeEntry(entryId); app.read; l }

      case UpdateFolderName(folder, newName) =>
        timed("UpdateFolderName") { val l = app.updateFolderName(folder, newName); app.read; l }

      case UpdateTaskListName(taskListId, newName) =>
        timed("UpdateTaskListName") { val l = app.updateTaskListName(taskListId, newName); app.read; l }

      case AddTask(taskListId, task) =>
        timed("AddTask") { val l = app.addTaskListItem(taskListId, task); app.read; l }

      case UpdateTaskDone(taskListId, itemIx) =>
        timed("UpdateTaskDone") {
          val l = app.updateTaskDone(
            taskListId,
            itemIx,
            done = true
          );
          app.read; l
        }

      case RemoveTaskListItem(taskListId, itemIx) =>
        timed("RemoveTaskListItem") { val l = app.removeTaskListItem(taskListId, itemIx); app.read; l }

      case MoveTaskListItem(taskListId, from, to) =>
        timed("MoveTaskListItem") { val l = app.moveTaskListItem(taskListId, from, to); app.read; l }

      case UpdateTaskTitle(taskListId, itemIx, newTitle) =>
        timed("UpdateTaskTitle") { val l = app.updateTaskTitle(taskListId, itemIx, newTitle); app.read; l }

      case UpdateTaskDescription(taskListId, itemIx, newDescription) =>
        timed("UpdateTaskDescription") {
          val l = app.updateTaskDescription(taskListId, itemIx, newDescription); app.read; l
        }

      case MarkItemsAsDoneThatMatch(taskListId, text) =>
        timed("MarkItemsAsDoneThatMatch") {
          val l = app.forEachTaskListItem(
            taskListId,
            task => if task.title.value.contains(text) then task.copy(done = LastWriterWins.now(true)) else task
          );
          app.read; l
        }

      case Undo =>
        timed("Undo") { val l = app.state.undo(); app.read; l }

      case Redo =>
        timed("Redo") { val l = app.state.redo(); app.read; l }
    }
    delta
  }
}
