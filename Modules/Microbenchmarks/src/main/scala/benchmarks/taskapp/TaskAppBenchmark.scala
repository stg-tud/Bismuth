package benchmarks.taskapp

import benchmarks.taskapp.TaskApp.{App, Entry, given}
import rdts.datatypes.ReplicatedTree
import rdts.base.LocalUid
import rdts.experiments.UndoRedoReplica
import rdts.experiments.DeltaHistory

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable

object TaskAppBenchmark {

  val numInteractions  = 1_000_000
  val checkpointEvery  = 100
  val progressLogEvery = 1_000

  // Two replicas with separate LocalUids
  val localUid1: LocalUid = LocalUid.predefined("benchmark-client-1")
  val localUid2: LocalUid = LocalUid.predefined("benchmark-client-2")

  var app1: App = scala.compiletime.uninitialized
  var app2: App = scala.compiletime.uninitialized

  // Track which replica is active (alternates between 1 and 2)
  var activeReplica: Int = 1

  // Timing trackers per operation type
  case class OpStats(var count: Long = 0, var totalNanos: Long = 0) {
    def avgMs: Double             = if count == 0 then 0.0 else totalNanos / (count * 1_000_000.0)
    def record(nanos: Long): Unit = {
      count += 1
      totalNanos += nanos
    }
    def reset(): Unit = {
      count = 0
      totalNanos = 0
    }
  }

  val opTimings: mutable.Map[String, OpStats] = mutable.Map(
    "AddFolder"             -> OpStats(),
    "AddTaskList"           -> OpStats(),
    "MoveEntry"             -> OpStats(),
    "RemoveEntry"           -> OpStats(),
    "UpdateFolderName"      -> OpStats(),
    "UpdateTaskListName"    -> OpStats(),
    "AddTask"               -> OpStats(),
    "RemoveTaskListItem"    -> OpStats(),
    "MoveTaskListItem"      -> OpStats(),
    "UpdateTaskTitle"       -> OpStats(),
    "UpdateTaskDescription" -> OpStats(),
    "Undo"                  -> OpStats(),
    "Redo"                  -> OpStats(),
    "read"                  -> OpStats(), // Track app.read calls separately
    "Sync"                  -> OpStats(), // Track sync between replicas
  )

  inline def timed[T](opName: String)(op: => T): T = {
    val start   = System.nanoTime()
    val result  = op
    val elapsed = System.nanoTime() - start
    opTimings(opName).record(elapsed)
    result
  }

  // Helper to get the current active app
  inline def currentApp: App      = if activeReplica == 1 then app1 else app2
  inline def otherApp: App        = if activeReplica == 1 then app2 else app1
  inline def currentUid: LocalUid = if activeReplica == 1 then localUid1 else localUid2

  inline def timedRead: ReplicatedTree[Entry] = timed("read")(currentApp.read)

  // Track sync time separately to exclude from interaction timing
  var syncTimeInCurrentBatch: Long = 0

  def main(args: Array[String]): Unit = {
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
      "interactions,treeSize,folderCount,taskListCount,totalTaskCount,last100InteractionsNanoTime,avgInteractionMs,syncNanoTime,avgSyncMs"
    )

    val startNanoTime: Long             = System.nanoTime()
    var lastCheckPointEndNanoTime: Long = startNanoTime

    var counter = 0

    println("Running benchmark interactions (alternating between two replicas)...")

    // First interaction creates a task list (on replica 1)
    activeReplica = 1
    performInteraction(generator.initialInteraction())
    counter += 1

    while counter < numInteractions do {
      // Alternate between replicas
      activeReplica = if activeReplica == 1 then 2 else 1

      val interaction = generator.nextInteraction(currentApp.read)
      performInteraction(interaction)
      counter += 1

      if counter % checkpointEvery == 0 then {
        val checkPointStartNanoTime        = System.nanoTime()
        val syncNanoTime                   = syncTimeInCurrentBatch
        val nanoTimeForLast100Interactions = checkPointStartNanoTime - lastCheckPointEndNanoTime - syncNanoTime
        syncTimeInCurrentBatch = 0 // Reset for next batch

        val tree                                         = currentApp.read
        val treeSize                                     = tree.size
        val (folderCount, taskListCount, totalTaskCount) = countEntries(tree)

        csvFile.println(
          s"$counter,$treeSize,$folderCount,$taskListCount,$totalTaskCount,$nanoTimeForLast100Interactions,${nanoTimeForLast100Interactions / (1_000_000.0 * checkpointEvery)},$syncNanoTime,${syncNanoTime / (1_000_000.0 * checkpointEvery)}"
        )

        if counter % progressLogEvery == 0 then {
          val historySize1 = app1.state.history.deltas.size
          val historySize2 = app2.state.history.deltas.size
          println(
            s"$counter/$numInteractions completed / avg: ${nanoTimeForLast100Interactions / (1_000_000.0 * checkpointEvery)}ms / tree: $treeSize / history deltas: $historySize1/$historySize2"
          )
          // Print per-operation timing breakdown
          println("  Operation timings (last 1000):")
          opTimings.toSeq.sortBy(-_._2.avgMs).foreach { case (op, stats) =>
            if stats.count > 0 then
                println(
                  f"    $op%-20s: ${stats.avgMs}%.4fms avg (${stats.count} calls, ${stats.totalNanos / 1_000_000.0}%.2fms total)"
                )
          }
          // Print Replica internal timing breakdown
          println(rdts.experiments.ReplicaTimings.report())
          rdts.experiments.ReplicaTimings.reset()
          // Print ReplicatedTree merge timing breakdown
          println(rdts.datatypes.ReplicatedTree.MergeTimings.report())
          rdts.datatypes.ReplicatedTree.MergeTimings.reset()
          // Reset timings for next batch
          opTimings.values.foreach(_.reset())
        }

        lastCheckPointEndNanoTime = System.nanoTime()
      }
    }

    csvFile.close()

    val totalTimeMs = (System.nanoTime() - startNanoTime) / 1_000_000.0
    println(s"Benchmark completed in ${totalTimeMs}ms")
    println(s"Total interactions: $counter")
    println(s"Results written to: ${csvFilePath.toAbsolutePath}")
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
          totalTaskCount += list.items.toList.size
      }
    }

    (folderCount, taskListCount, totalTaskCount)
  }

  private def performInteraction(interaction: TaskAppInteraction): Unit = {
    given LocalUid = currentUid
    val app        = currentApp
    val other      = otherApp

    val delta: DeltaHistory[ReplicatedTree[Entry]] = interaction match {
      case AddFolder(parentFolder, name) =>
        timed("AddFolder") { app.addFolder(parentFolder, name) }

      case AddTaskList(parentFolder, name) =>
        timed("AddTaskList") { app.addTaskList(parentFolder, name) }

      case MoveEntry(entryId, newParent) =>
        timed("MoveEntry") { app.moveEntry(entryId, newParent) }

      case RemoveEntry(entryId) =>
        timed("RemoveEntry") { app.removeEntry(entryId) }

      case UpdateFolderName(folder, newName) =>
        timed("UpdateFolderName") { app.updateFolderName(folder, newName) }

      case UpdateTaskListName(taskListId, newName) =>
        timed("UpdateTaskListName") { app.updateTaskListName(taskListId, newName) }

      case AddTask(taskListId, task) =>
        timed("AddTask") { app.addTaskListItem(taskListId, task) }

      case RemoveTaskListItem(taskListId, itemIx) =>
        timed("RemoveTaskListItem") { app.removeTaskListItem(taskListId, itemIx) }

      case MoveTaskListItem(taskListId, from, to) =>
        timed("MoveTaskListItem") { app.moveTaskListItem(taskListId, from, to) }

      case UpdateTaskTitle(taskListId, itemIx, newTitle) =>
        timed("UpdateTaskTitle") { app.updateTaskTitle(taskListId, itemIx, newTitle) }

      case UpdateTaskDescription(taskListId, itemIx, newDescription) =>
        timed("UpdateTaskDescription") { app.updateTaskDescription(taskListId, itemIx, newDescription) }

      case Undo =>
        timed("Undo") { app.state.undo() }

      case Redo =>
        timed("Redo") { app.state.redo() }
    }

    // Sync the delta to the other replica immediately (timed separately, excluded from interaction timing)
    val syncStart   = System.nanoTime()
    val _           = other.applyDelta(delta)
    val syncElapsed = System.nanoTime() - syncStart
    opTimings("Sync").record(syncElapsed)
    syncTimeInCurrentBatch += syncElapsed
  }
}
