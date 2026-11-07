package scripts

import java.nio.file.{Files, Path}
import scala.util.matching.Regex

def selectScheduler(selection: String): Unit =
    val targetFolder = Path.of("Modules/Reactives/src/main/scala/reactives/scheduler/").toAbsolutePath
    val sourceFile   = targetFolder.resolve("GlobalCandidate.scala")

    if !Files.isRegularFile(sourceFile) then
        throw IllegalStateException(s"could not find regular file: $sourceFile")

    val regex      = Regex(raw"""(?x)(?s) case \s+ " (?<candidate> \w+ ) " \s+ =>""")
    val candidates = regex.findAllMatchIn(Files.readString(sourceFile)).map(_.group("candidate")).toList

    if !candidates.contains(selection) then
        println(s"invalid selection: »$selection«, the following are available:")
        candidates.foreach(println)
    else
        Files.writeString(
          targetFolder.resolve("GeneratedSelection.scala"),
          s"""package reactives.scheduler
             |object GeneratedSelection {
             |  inline val selection: "$selection" = "$selection"
             |}
             |""".stripMargin
        ): Unit
