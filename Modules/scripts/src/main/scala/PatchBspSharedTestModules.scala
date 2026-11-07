package scripts

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.matching.Regex

/** IntelliJ's BSP importer drops the module dependencies of the synthetic "shared sources" test modules of a
  * project matrix: it maps a BSP dependency target onto the platform module (rdts/rdtsJS/rdtsNative) instead of the
  * shared module, and for test modules that edge would close a cycle (rdts -> rdts...test (shared) -> rdts), so it
  * is discarded entirely. See https://youtrack.jetbrains.com/issue/SCL-23703
  *
  * This adds the missing (acyclic) edges to the shared modules by hand, derived from the platform modules, which
  * still have correct edges: for every shared test module T, its platform modules P (those depending on T) also
  * depend on the shared main module of the project, and on the platform modules Q of upstream projects. T gets the
  * shared main module, plus the shared modules (main and test) of every Q. We cannot tell compile->compile from
  * test->test edges from the iml files, so upstream shared test modules are always added; that is acyclic and at
  * worst adds a few unused test sources.
  *
  * Idempotent, and undone by the next BSP reimport. Run with the project closed, otherwise IntelliJ overwrites the
  * files again.
  */
def patchBspSharedTestModules(dryRun: Boolean): Unit =
    val modulesDir = Path.of(".idea/modules").toAbsolutePath
    if !Files.isDirectory(modulesDir) then
        throw IllegalStateException(s"no such directory: $modulesDir")

    val moduleEntry = Regex("""<orderEntry type="module" module-name="([^"]+)"""")

    val imls: Map[String, String] =
        Files.list(modulesDir).iterator().asScala
            .filter(_.getFileName.toString.endsWith(".iml"))
            .map(p => p.getFileName.toString.stripSuffix(".iml") -> Files.readString(p))
            .toMap

    def deps(module: String): Set[String] =
        imls.get(module).iterator.flatMap(moduleEntry.findAllMatchIn(_).map(_.group(1))).toSet

    def isShared(module: String) = module.endsWith(" (shared)")

    val testModules = imls.keys.filter(_.endsWith("(test+-)test (shared)")).toList.sorted

    for testModule <- testModules do
        val platforms = imls.keys.filter(m => !isShared(m) && deps(m).contains(testModule))
        val wanted    = platforms.flatMap(deps).filter(_ != testModule).flatMap { dep =>
            if isShared(dep) then Set(dep)
            else deps(dep).filter(isShared)
        }.toList.sorted

        if wanted.nonEmpty then
            println(s"patching $testModule")
            val present = deps(testModule)
            val missing = wanted.filterNot(present.contains)
            wanted.filter(present.contains).foreach(name => println(s"  already depends on $name"))
            missing.foreach(name => println(s"  ${if dryRun then "would add" else "added"} $name"))

            if !dryRun && missing.nonEmpty then
                val content = imls(testModule)
                // insert right before the end of the single <component> element
                val end = content.lastIndexOf("  </component>")
                if end < 0 then throw IllegalStateException(s"no </component> in $testModule")
                val entries = missing.map(name =>
                    s"""    <orderEntry type="module" module-name="$name" scope="TEST" />\n"""
                ).mkString
                Files.writeString(
                  modulesDir.resolve(s"$testModule.iml"),
                  content.substring(0, end) + entries + content.substring(end)
                ): Unit
