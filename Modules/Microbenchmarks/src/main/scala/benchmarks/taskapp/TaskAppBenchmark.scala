package benchmarks.taskapp

import benchmarks.taskapp.TaskApp.{App, Entry, given}
import rdts.datatypes.ReplicatedTree
import rdts.base.LocalUid
import rdts.experiments.Replica as UndoRedoReplica

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable

object TaskAppBenchmark {

  val numInteractions  = 1_000_000
  val pruningThreshold = 50
  val keptTasks        = 20
  val checkpointEvery  = 100
  val progressLogEvery = 1_000

  given localUid: LocalUid = LocalUid.predefined("benchmark-client")

  var app: App = scala.compiletime.uninitialized

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
  )

  inline def timed[T](opName: String)(op: => T): T = {
    val start   = System.nanoTime()
    val result  = op
    val elapsed = System.nanoTime() - start
    opTimings(opName).record(elapsed)
    result
  }

  inline def timedRead: ReplicatedTree[Entry] = timed("read")(app.read)

  def main(args: Array[String]): Unit = {
    println("Starting TaskApp benchmark...")

    // Initialize the app with an empty Replica
    app = App(UndoRedoReplica.empty[ReplicatedTree[Entry]])

    // Create the live interaction generator
    val generator = new TaskAppInteractionGenerator(pruningThreshold, keptTasks)

    // Setup CSV output
    val csvFilePath: Path = Paths.get("./benchmarks/results/taskapp_benchmark.csv")
    Files.createDirectories(csvFilePath.getParent)
    val csvFile = new PrintWriter(csvFilePath.toFile)

    csvFile.println(
      "interactions,treeSize,folderCount,taskListCount,totalTaskCount,last100InteractionsNanoTime,avgInteractionMs"
    )

    val startNanoTime: Long             = System.nanoTime()
    var lastCheckPointEndNanoTime: Long = startNanoTime

    var counter = 0

    println("Running benchmark interactions...")

    // First interaction creates a task list
    performInteraction(generator.initialInteraction())
    counter += 1

    while counter < numInteractions do {
      val interaction = generator.nextInteraction(app.read)
      performInteraction(interaction)
      counter += 1

      if counter % checkpointEvery == 0 then {
        val checkPointStartNanoTime        = System.nanoTime()
        val nanoTimeForLast100Interactions = checkPointStartNanoTime - lastCheckPointEndNanoTime

        val tree                                         = app.read
        val treeSize                                     = tree.size
        val (folderCount, taskListCount, totalTaskCount) = countEntries(tree)

        csvFile.println(
          s"$counter,$treeSize,$folderCount,$taskListCount,$totalTaskCount,$nanoTimeForLast100Interactions,${nanoTimeForLast100Interactions / (1_000_000.0 * checkpointEvery)}"
        )

        if counter % progressLogEvery == 0 then {
          val historySize = app.state.history.deltas.size
          println(
            s"$counter/$numInteractions completed / avg: ${nanoTimeForLast100Interactions / (1_000_000.0 * checkpointEvery)}ms / tree: $treeSize / history deltas: $historySize"
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
    interaction match {
      case AddFolder(parentFolder, name) =>
        val _ = timed("AddFolder") { app.addFolder(parentFolder, name) }

      case AddTaskList(parentFolder, name) =>
        val _ = timed("AddTaskList") { app.addTaskList(parentFolder, name) }

      case MoveEntry(entryId, newParent) =>
        val _ = timed("MoveEntry") { app.moveEntry(entryId, newParent) }

      case UpdateFolderName(folder, newName) =>
        val _ = timed("UpdateFolderName") { app.updateFolderName(folder, newName) }

      case UpdateTaskListName(taskListId, newName) =>
        val _ = timed("UpdateTaskListName") { app.updateTaskListName(taskListId, newName) }

      case AddTask(taskListId, task) =>
        val _ = timed("AddTask") { app.addTaskListItem(taskListId, task) }

      case RemoveTaskListItem(taskListId, itemIx) =>
        val _ = timed("RemoveTaskListItem") { app.removeTaskListItem(taskListId, itemIx) }

      case MoveTaskListItem(taskListId, from, to) =>
        val _ = timed("MoveTaskListItem") { app.moveTaskListItem(taskListId, from, to) }

      case UpdateTaskTitle(taskListId, itemIx, newTitle) =>
        val _ = timed("UpdateTaskTitle") { app.updateTaskTitle(taskListId, itemIx, newTitle) }

      case UpdateTaskDescription(taskListId, itemIx, newDescription) =>
        val _ = timed("UpdateTaskDescription") { app.updateTaskDescription(taskListId, itemIx, newDescription) }

      case Undo =>
        val _ = timed("Undo") { app.state.undo() }

      case Redo =>
        val _ = timed("Redo") { app.state.redo() }
    }
  }
}
