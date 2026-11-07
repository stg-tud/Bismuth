/* Connects to sbt via BSP, compiles all targets, and removes all imports reported as unused.
 *
 * Only single-line imports are rewritten, everything else is reported. */

package scripts

import ch.epfl.scala.bsp4j.*
import de.rmgk.script.*
import org.eclipse.lsp4j.jsonrpc.Launcher

import java.net.URI
import java.nio.file.{Files, Path}
import java.util.concurrent.Executors
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

val root = Path.of(".").toAbsolutePath.normalize

/** Unused import positions: file -> line -> columns. */
type Hits = Map[Path, Map[Int, Set[Int]]]

class Client extends BuildClient {
    private val diagnostics = mutable.Map.empty[(String, String), List[Diagnostic]]

    def hits: Hits = synchronized {
        val positions = for
            ((uri, _), diags) <- diagnostics.toList
            d                 <- diags if d.getMessage.toLowerCase.contains("unused import")
        yield (Path.of(URI(uri)), d.getRange.getStart)
        positions.groupBy(_._1).view.mapValues { ps =>
            ps.map(_._2).groupMap(_.getLine.toInt)(_.getCharacter.toInt).view.mapValues(_.toSet).toMap
        }.toMap
    }

    override def onBuildPublishDiagnostics(p: PublishDiagnosticsParams): Unit = synchronized {
        val key = (p.getTextDocument.getUri, p.getBuildTarget.getUri)
        val old = if p.getReset then Nil else diagnostics.getOrElse(key, Nil)
        diagnostics(key) = old ++ p.getDiagnostics.asScala
    }

    override def onBuildLogMessage(p: LogMessageParams): Unit             = System.err.println(s"  [sbt] ${p.getMessage}")
    override def onBuildShowMessage(p: ShowMessageParams): Unit           = ()
    override def onBuildTaskStart(p: TaskStartParams): Unit               = ()
    override def onBuildTaskProgress(p: TaskProgressParams): Unit         = ()
    override def onBuildTaskFinish(p: TaskFinishParams): Unit             = ()
    override def onBuildTargetDidChange(p: DidChangeBuildTarget): Unit    = ()
}

trait Server extends BuildServer with ScalaBuildServer

def collectUnusedImports(clean: Boolean): Hits =
    val argv    = ujson.read(Files.readString(root.resolve(".bsp/sbt.json")))("argv").arr.map(_.str)
    val process = process"$argv".directory(root.toFile).redirectError(ProcessBuilder.Redirect.INHERIT).start()
    val client  = Client()
    val pool    = Executors.newCachedThreadPool()
    val launcher = Launcher.Builder[Server]()
        .setRemoteInterface(classOf[Server])
        .setLocalService(client)
        .setInput(process.getInputStream)
        .setOutput(process.getOutputStream)
        .setExecutorService(pool)
        .create()
    val listening = launcher.startListening()
    val server = launcher.getRemoteProxy

    val capabilities = BuildClientCapabilities(List("scala").asJava)
    server.buildInitialize(InitializeBuildParams("fix-unused-imports", "0.1", "2.1.0", root.toUri.toString, capabilities)).get
    server.onBuildInitialized()

    val targets = server.workspaceBuildTargets.get.getTargets.asScala.filter(_.getLanguageIds.contains("scala")).map(_.getId).asJava
    System.err.println(s"${targets.size} targets")
    if clean then server.buildTargetCleanCache(CleanCacheParams(targets)).get: Unit
    System.err.println(s"compile: ${server.buildTargetCompile(CompileParams(targets)).get.getStatusCode}")

    server.buildShutdown().get
    server.onBuildExit()
    process.waitFor(10, java.util.concurrent.TimeUnit.SECONDS)
    process.destroy()
    listening.cancel(true)
    pool.shutdownNow()
    client.hits

/** A comma separated element of an import line, with its position in that line. */
case class Part(start: Int, text: String) {
    def end              = start + text.length
    def covers(col: Int) = start <= col && col <= end
}

/** Splits on commas that are not nested in braces. */
def splitTop(s: String, offset: Int): List[Part] =
    var depth = 0
    val cuts = s.indices.filter { i =>
        if s(i) == '{' then depth += 1
        if s(i) == '}' then depth -= 1
        s(i) == ',' && depth == 0
    }
    val bounds = (-1 +: cuts :+ s.length).sliding(2).toList
    bounds.map { case Seq(from, to) =>
        val raw = s.substring(from + 1, to)
        Part(offset + from + 1 + raw.indexWhere(!_.isWhitespace).max(0), raw.trim)
    }

val ImportLine = """(\s*import\s+)([^/]*?)(\s*)""".r

/** Removes the selectors at the given columns. None means the line can not be handled, Some(None) means it is gone. */
def rewrite(line: String, cols: Set[Int]): Option[Option[String]] = line match
    case ImportLine(prefix, body, _) if body.count(_ == '{') == body.count(_ == '}') =>
        val matched = mutable.Set.empty[Int]
        def unused(p: Part) =
            val hit = cols.filter(p.covers)
            matched ++= hit
            hit.nonEmpty

        val kept = splitTop(body, prefix.length).flatMap { clause =>
            clause.text.indexOf('{') match
                case brace if brace >= 0 && clause.text.endsWith("}") =>
                    val selectors = splitTop(clause.text.substring(brace + 1, clause.text.length - 1), clause.start + brace + 1)
                    selectors.filterNot(unused) match
                        case Nil                                   => None
                        case rest if rest.size == selectors.size => Some(clause.text)
                        case rest => Some(rest.map(_.text).mkString(s"${clause.text.take(brace)}{", ", ", "}"))
                case _ =>
                    val name = clause.text.lastIndexOf('.') + 1
                    Option.unless(unused(Part(clause.start + name, clause.text.drop(name))))(clause.text)
        }
        Option.when(matched == cols)(Option.when(kept.nonEmpty)(kept.mkString(prefix, ", ", "")))
    case _ => None

def fixUnusedImports(clean: Boolean, dryRun: Boolean): Unit =
    val hits  = collectUnusedImports(clean)
    var fixed = 0
    for (file, byLine) <- hits.toList.sortBy(_._1) do
        val lines = Files.readAllLines(file).asScala.toVector
        val results = byLine.map((n, cols) => n -> rewrite(lines(n), cols))
        for case (n, None) <- results do System.err.println(s"cannot fix: ${root.relativize(file)}:${n + 1}: ${lines(n).trim}")
        val updates = results.collect { case (n, Some(r)) => n -> r }
        if updates.nonEmpty then
            fixed += updates.size
            println(s"${root.relativize(file)}: ${updates.size} line(s) fixed")
            val newLines = lines.indices.flatMap(n => updates.getOrElse(n, Some(lines(n))))
            if !dryRun then Files.writeString(file, newLines.mkString("", "\n", "\n")): Unit

    System.err.println(if hits.isEmpty then "no unused imports found (try --clean)" else s"$fixed import line(s) fixed")
