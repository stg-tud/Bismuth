package benchmarks.encrdt

import benchmarks.encrdt.Codecs.given
import benchmarks.encrdt.mock.insecure.{AlternativeInsecureToDoListClient, AlternativeInsecureToDoListIntermediary}
import benchmarks.encrdt.mock.{DisseminationStats, IntermediarySizeInfo, SecureToDoListClient, ToDoListClient, ToDoListIntermediary}
import benchmarks.encrdt.todolist.{AddToDoItem, CompleteToDoItem, RemoveToDoItems, ToDoEntry, ToDoListInteraction, ToDoListInteractionGenerator}
import rdts.syntax.oldCompat.DeltaAWLWWMContainer

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}
import java.util.UUID

object ToDoAppBenchmark {

  val USE_ENCRYPTION = true

  val numInteractions  = 1_000_000
  val pruningThreshold = 50
  val keptToDos        = 20

  val interactions: Iterable[ToDoListInteraction] =
    new ToDoListInteractionGenerator(pruningThreshold, keptToDos).generateInteractions(numInteractions)

  val clientCrdt = new DeltaAWLWWMContainer[UUID, ToDoEntry]("client".convert)

  var intermediarySizeInfo: IntermediarySizeInfo = scala.compiletime.uninitialized
  var aead: replication.Aead                     = scala.compiletime.uninitialized
  var clientReplica: ToDoListClient              = scala.compiletime.uninitialized

  def main(args: Array[String]): Unit = {

    if USE_ENCRYPTION then {
      aead = AeadTranslation(Helper.setupAead("AES128_GCM"))
      val intermediaryReplica = new ToDoListIntermediary
      intermediarySizeInfo = intermediaryReplica
      clientReplica = new SecureToDoListClient("client".convert, clientCrdt, aead, intermediaryReplica)
    } else {
      val intermediaryReplica = new AlternativeInsecureToDoListIntermediary("intermediary")
      intermediarySizeInfo = intermediaryReplica
      clientReplica = new AlternativeInsecureToDoListClient("client", clientCrdt, intermediaryReplica)
    }

    val csvFileF: Path =
      if USE_ENCRYPTION then Paths.get("./benchmarks/results/todoapp_benchmark.csv")
      else Paths.get("./benchmarks/results/todoapp_benchmark trusted intermediary.csv")
    Files.createDirectories(csvFileF.getParent)
    val csvFile = new PrintWriter(csvFileF.toFile)
    csvFile.println(
      "interactions,intermediarySize,encDeltaCausalitySize,encDeltaCiphertextSize,intermediaryStoredDeltas,completedToDos,uncompletedToDos,last100InteractionsNanoTime,last100InteractionsDisseminatedBytes,last100InteractionsAdditionDisseminatedBytes,last100InteractionsCompletionDisseminatedBytes,last100InteractionsRemovalBytes"
    )

    var lastDisseminationStats: DisseminationStats = DisseminationStats(0, 0, 0, 0)

    val startNanoTime: Long             = System.nanoTime()
    var lastCheckPointEndNanoTime: Long = startNanoTime

    var counter = 0
    interactions.foreach { interaction =>
      performInteraction(interaction, clientReplica)
      counter += 1

      if counter % 100 == 0 then {
        val checkPointStartNanoTime        = System.nanoTime()
        val nanoTimeForLast100Interactions = checkPointStartNanoTime - lastCheckPointEndNanoTime
        val last100DisseminationStatDiff   = clientReplica.disseminationStats - lastDisseminationStats
        lastDisseminationStats = clientReplica.disseminationStats

        val storedDeltasOnIntermediary = intermediarySizeInfo.numberStoredDeltas
        val causalitySize              = intermediarySizeInfo.encDeltaCausalityInfoSizeInBytes
        val rawDeltaSize               = intermediarySizeInfo.rawDeltasSizeInBytes

        val entries            = clientCrdt.values
        val completedEntries   = entries.filter(_._2.completed)
        val uncompletedEntries = entries.filterNot(_._2.completed)

        csvFile.println(
          s"$counter,${causalitySize + rawDeltaSize},$causalitySize,$rawDeltaSize,$storedDeltasOnIntermediary,${completedEntries.size},${uncompletedEntries.size},$nanoTimeForLast100Interactions,${last100DisseminationStatDiff.total},${last100DisseminationStatDiff.addition},${last100DisseminationStatDiff.completion},${last100DisseminationStatDiff.removal}"
        )

        if counter % 1_000 == 0 then {
          println(
            s"$counter/$numInteractions interactions completed / avg over last 100: ${(checkPointStartNanoTime - lastCheckPointEndNanoTime) / (1_000_000.0 * 100)}ms"
          )
        }

        lastCheckPointEndNanoTime = System.nanoTime()
      }
    }

    csvFile.close()

    println("Overall " + clientReplica.disseminationStats)
  }

  private def performInteraction(interaction: ToDoListInteraction, clientReplica: ToDoListClient): Unit =
    interaction match {
      case AddToDoItem(uuid, toDoEntry) =>
        clientReplica.addToDoItem(uuid, toDoEntry)

      case CompleteToDoItem(uuid) =>
        clientReplica.completeToDoItem(uuid)

      case RemoveToDoItems(uuids) =>
        clientReplica.removeToDoItems(uuids)
    }
}
