package loreCompilerPlugin

import dotty.tools.dotc.{CompilationUnit, report}
import dotty.tools.dotc.ast.Trees.{Block, Tree, Literal}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.{Inlining, Pickler}
import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.dotc.util.Spans.Span
import java.io.File // For getting URIs and the system-independent path separator
import lore.ast.Term
import loreCompilerPlugin.codegen.LoReGen.*
import loreCompilerPlugin.codegen.DafnyGen.generate as generateDafnyCode
import loreCompilerPlugin.lsp.DafnyLSPClient
import loreCompilerPlugin.lsp.LSPDataTypes.*
import ujson.Obj

import scala.annotation.nowarn

enum LogLevel(val level: Int) {
  case None      extends LogLevel(0)
  case Essential extends LogLevel(1)
  case Sparse    extends LogLevel(2)
  case Verbose   extends LogLevel(3)

  /** Is this log level equal to or higher than the given log level?
    * @param level The log level to compare to
    * @return Whether this log level is equal to or higher than the given log levl.
    */
  def isLevelOrHigher(level: LogLevel): Boolean = {
    this.level >= level.level
  }
}

class LoRePlugin extends StandardPlugin {
  val name: String        = "LoRe Compiler Plugin"
  val description: String = "Verifies Scala-embedded LoRe code through Dafny"

  @nowarn // which variant to override depends on the scala version, use the old one until 3.5 is more stable
  override def init(options: List[String]): List[PluginPhase] = List(new LoRePhase)
}

object LoRePhase {
  val name: String        = "LoRe"
  val description: String = "verifies LoRe source code through compilation to Dafny"
}

class LoRePhase extends PluginPhase {
  val phaseName: String                = LoRePhase.name
  override val description: String     = LoRePhase.description
  override val runsAfter: Set[String]  = Set(Pickler.name)
  override val runsBefore: Set[String] = Set(Inlining.name)

  private given logLevel: LogLevel = LogLevel.Essential
  // Generated LoRe ASTs per method
  private var loreTerms: Map[(SourceFile, Name), List[Term]] = Map()

  override def transformDefDef(tree: tpd.DefDef)(using ctx: Context): tpd.Tree = {
    // Only process DefDefs marked as LoreProgram
    val loreAnnotation = tree.symbol.annotations.find(annot => annot.symbol.name.toString == "LoReProgram")

    if loreAnnotation.isDefined then {
      var programTermList: List[Term] = List()

      val programContents: List[Tree[?]] =
        tree.rhs match
          case block: Block[?] =>
            // All of the block's trees, apart from the very last (which is used as return value), are included
            // in the "stats" property. We also want the last one however, so add it to the list manually.
            // The exception to this is when the last tree is a definition, where expr is an Constant Literal of Unit.
            block.expr match
              case Literal(Constant(_: Unit)) => block.stats
              case _                          => block.stats :+ block.expr
          case _ => List()

      // Process each individual part of this LoRe program
      for t <- programContents do {
        // Generate LoRe term for this tree
        val loreTerm: List[Term] = createLoreTermFromTree(t, programTermList)

        // Add generated LoRe term to this LoreProgram's term list
        programTermList = programTermList :++ loreTerm
      }

      // Add the finalized term list for this LoreProgram to the phase's overall term list
      if logLevel.isLevelOrHigher(LogLevel.Sparse) then {
        println(s"Adding new term list for ${tree.name} in ${tree.source.name}.")
      }
      loreTerms = loreTerms.updated((tree.source, tree.name), programTermList)
    }

    // Original Scala tree is returned for regular Scala compilation to proceed
    tree
  }

  override def runOn(units: List[CompilationUnit])(using ctx: Context): List[CompilationUnit] = {
    if logLevel.isLevelOrHigher(LogLevel.Essential) then {
      println("Generating LoRe AST nodes from Scala code...")
    }
    // First, run the runOn method for regular Scala compilation (do not remove this or this phase breaks).
    // This will cause the compiler to call above-defined methods which will generate the LoRe AST nodes.
    val result = super.runOn(units)

    val termCounts: String = loreTerms.map(_._2.size).mkString(",")
    if logLevel.isLevelOrHigher(LogLevel.Sparse) then {
      println(s"LoRe AST generation completed. Processed ${loreTerms.size} files with $termCounts terms each.")
    }

    // Initialize LSP client that will be used for verification after codegen
    val lspClient: DafnyLSPClient = new DafnyLSPClient(logLevel)

    // Get the root path of the project these compilation units are from.
    // Basically, cut off anything that comes after "src/main/scala".
    // Have to do some messing around with escaping slashes and URIs here.
    val unitPath: String    = units.head.source.path
    val rootPattern: String = List("src", "main", "scala").mkString(File.separator) // Windows/Unix separators
    val rootPatternEscaped: String = rootPattern.replace("\\", "\\\\") // Gotta escape since split takes regex
    // Take the first half of the split, then add the split off separator back on and get the path as an URI string
    val folderPath: String = File(unitPath.split(rootPatternEscaped).head.concat(rootPattern)).toURI.toString
    lspClient.initializeLSP(folderPath)

    var programsWithErrors: List[String] = List()

    // Iterate through all term lists and generate Dafny code for them + verify it
    for ((file, method), terms) <- loreTerms do {
      if logLevel.isLevelOrHigher(LogLevel.Essential) then {
        println(s"\nGenerating Dafny code from LoRe AST of ${method.toString}...")
      }

      // Turn the filepath into an URI and then sneakily change the file extension the LSP gets to see
      val filePath: String = File(file.path).toURI.toString.replace(".scala", ".dfy")
      val fileName: String = filePath.substring(filePath.lastIndexOf("/") + 1)

      // Generate Dafny code from term list
      val dafnyCode: String        = generateDafnyCode(terms, method.toString)
      val dafnyLines: List[String] = dafnyCode.linesIterator.toList

      if logLevel.isLevelOrHigher(LogLevel.Essential) then {
        println(s"Dafny code generation for ${method.toString} completed.")
        println(s"Compiling and verifying Dafny code for ${method.toString}...")
      }

      // Send the generated code "file" to the language server to verify
      val didOpenMessage: String = DafnyLSPClient.constructLSPMessage("textDocument/didOpen")(
        (
          "textDocument",
          Obj(
            "uri"        -> filePath,
            "languageId" -> "dafny",
            "version"    -> 1,
            "text"       -> dafnyCode
          )
        ),
      )
      lspClient.sendMessage(didOpenMessage)

      // Wait for verification results and then filter out any verification errors that occurred
      val (
        verificationResult: SymbolStatusNotification,
        diagnosticsNotification: Option[PublishDiagnosticsNotification]
      ) =
        lspClient.waitForVerificationResult(fileName)

      // If the wait for verification results was halted prematurely, a critical Dafny compilation error occurred.
      if verificationResult.params == null then {
        programsWithErrors = programsWithErrors :+ method.toString

        diagnosticsNotification match
          // No diagnostics were read before the error occurred: No error info known.
          case None       => report.error("An unknown critical Dafny compilation error occurred.")
          case Some(diag) =>
            // TODO: Dig into diagnostics to report error details
            val errors: List[Diagnostic] = diag.params.diagnostics.filter(d => {
              d.severity.isDefined && d.severity.get == DiagnosticSeverity.Error
            })

            val errorPlural: String = if errors.size > 1 then "errors" else "error"

            report.error(
              s"""${errors.size} Dafny compilation $errorPlural occurred:
                 |${errors.map(e => e.message).mkString("\n")}""".stripMargin
            )
      } else {
        // Regular processing of verification results.
        val erroneousVerifiables: List[NamedVerifiable] =
          verificationResult.params.namedVerifiables.filter(nv => nv.status == VerificationStatus.Error)

        // Process potential verification errors
        if erroneousVerifiables.isEmpty then {
          if logLevel.isLevelOrHigher(LogLevel.Essential) then {
            println(s"All verifiables in ${method.toString} were verified successfully.")
          }
        } else {
          programsWithErrors = programsWithErrors :+ method.toString

          val unverifiableNames: List[String] = erroneousVerifiables.map(verifiable => {
            // The names of verifiables (e.g. method names) will be on one line, so no need to check across
            val startChar: Int = verifiable.nameRange.start.character
            val endChar: Int   = verifiable.nameRange.end.character
            dafnyLines(verifiable.nameRange.start.line).substring(startChar, endChar)
          })

          diagnosticsNotification match
            case None =>
              report.error(
                s"""The following verifiables in ${method.toString} could not be verified:
                  |${unverifiableNames.mkString("\n")}""".stripMargin
              )
            case Some(diag) =>
              // Order of diagnostics and named verifiables lists is the same so we can associate by zipping
              diag.params.diagnostics.zip(unverifiableNames).foreach((d, n) => {
                d.relatedInformation match
                  case None =>
                    // No direct Scala position known
                    report.error(s"A compilation/verification error occurred in $n.")
                  case Some(info) =>
                    info.foreach(i => {
                      val scalaPos: Option[Long] = i.message.toLongOption

                      scalaPos match
                        case None =>
                          // Message isn't an encoded Scala position, so position unknown
                          report.error(s"A compilation/verification error occurred in $n.")
                        case Some(pos) =>
                          val sourcePos: SourcePosition = SourcePosition(file, new Span(pos))
                          report.error(d.message, sourcePos)
                    })
              })
        }
      }

      // Close the "file"
      val didCloseMessage: String = DafnyLSPClient.constructLSPMessage("textDocument/didClose")(
        ("textDocument", Obj("uri" -> filePath))
      )
      lspClient.sendMessage(didCloseMessage)
    }

    if programsWithErrors.nonEmpty then {
      if logLevel.isLevelOrHigher(LogLevel.Essential) then {
        val errorPlural: String = if programsWithErrors.size > 1 then "programs" else "program"
        println(
          s"""\n${programsWithErrors.length} LoRe $errorPlural exhibited syntax or verification errors:
             |${programsWithErrors.mkString("\n")}""".stripMargin
        )
      }
    } else {
      if logLevel.isLevelOrHigher(LogLevel.Essential) then {
        println("\nAll LoRe programs verified successfully.")
      }
    }

    // Always return result of default runOn method for regular Scala compilation, as we do not modify it.
    result
  }
}
