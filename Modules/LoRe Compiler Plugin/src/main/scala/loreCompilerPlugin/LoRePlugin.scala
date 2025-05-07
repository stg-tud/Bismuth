package loreCompilerPlugin

import dotty.tools.dotc.{CompilationUnit, report}
import dotty.tools.dotc.ast.Trees.{Block, Tree, Literal}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.{Inlining, Pickler}
import dotty.tools.dotc.util.SourceFile
import java.io.File // For getting URIs and the system-independent path separator
import lore.ast.Term
import loreCompilerPlugin.codegen.LoReGen.*
import loreCompilerPlugin.codegen.DafnyGen.generate as generateDafnyCode
import loreCompilerPlugin.lsp.DafnyLSPClient
import loreCompilerPlugin.lsp.LSPDataTypes.*
import ujson.Obj

import scala.annotation.nowarn

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

  private var loreTerms: Map[(SourceFile, Name), List[Term]] = Map()

  override def transformDefDef(tree: tpd.DefDef)(using ctx: Context): tpd.Tree = {
    // Only process DefDefs marked as LoreProgram
    val loreAnnotation = tree.symbol.annotations.find(annot => annot.symbol.name.toString == "LoReProgram")

    if loreAnnotation.isDefined then {
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
        val loreTerm: List[Term] = createLoreTermFromTree(t)

        // Add LoRe term to the respective program's term list, or create a term list if it doesn't exist yet
        val programTermList: Option[List[Term]] = loreTerms.get((tree.source, tree.name))
        programTermList match
          case Some(list) =>
            val newList: List[Term] = list :++ loreTerm // Append list with loreTerm list
            loreTerms = loreTerms.updated((tree.source, tree.name), newList)
          case None =>
            println(s"Creating new term list for ${tree.name} in ${tree.source.name}.")
            val newList: List[Term] = loreTerm // loreTerm is already a list
            loreTerms = loreTerms.updated((tree.source, tree.name), newList)
      }
    }

    // Original Scala tree is returned for regular Scala compilation to proceed
    tree
  }

  override def runOn(units: List[CompilationUnit])(using ctx: Context): List[CompilationUnit] = {
    println("Generating LoRe AST nodes from Scala code...")
    // First, run the runOn method for regular Scala compilation (do not remove this or this phase breaks).
    // This will cause the compiler to call above-defined methods which will generate the LoRe AST nodes.
    val result = super.runOn(units)

    val termCounts: String = loreTerms.map(_._2.size).mkString(",")
    println(s"LoRe AST generation completed. Processed ${loreTerms.size} files with $termCounts terms each.")

    // Initialize LSP client that will be used for verification after codegen
    val lspClient: DafnyLSPClient = new DafnyLSPClient()

    // Get the root path of the project these compilation units are from.
    // Basically, cut off anything that comes after "src/main/scala".
    // Have to do some messing around with escaping slashes and URIs here.
    val unitPath: String    = units.head.source.path
    val rootPattern: String = List("src", "main", "scala").mkString(File.separator) // Windows/Unix separators
    val rootPatternEscaped: String = rootPattern.replace("\\", "\\\\") // Gotta escape since split takes regex
    // Take the first half of the split, then add the split off separator back on and get the path as an URI string
    val folderPath: String = File(unitPath.split(rootPatternEscaped).head.concat(rootPattern)).toURI.toString
    lspClient.initializeLSP(folderPath)

    // TODO: dummy variable, remove later
    var counter: Int = 0

    // Iterate through all term lists and generate Dafny code for them + verify it
    for ((file, method), terms) <- loreTerms do {
      println(s"\nGenerating Dafny code from LoRe AST of ${method.toString}...")

      // Turn the filepath into an URI and then sneakily change the file extension the LSP gets to see
      val filePath: String = File(file.path).toURI.toString.replace(".scala", ".dfy")

      // Generate Dafny code from term list
      val dafnyCode: String        = generateDafnyCode(terms, method.toString)
      val dafnyLines: List[String] = dafnyCode.linesIterator.toList

      println(s"Dafny code generation for ${method.toString} completed.")

      counter += 1
      // todo: this is dummy code, normally output by the to-be-implemented dafny generator
      val dafnyDummyCode: String =
        s"""method Test(x: int) returns (y: int)
           |  ensures {:error "Error on LoRe ln X, col Y"} y == ${if counter == 1 then "x" else counter}
           |  {
           |    y := x;
           |  }
           |
           |method Main()
           |{
           |  var a: int := Test(0);
           |  print a;
           |}""".stripMargin

      println(s"Compiling and verifying Dafny code for ${method.toString}...")

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
        lspClient.waitForVerificationResult()

      // If the wait for verification results was halted prematurely, a critical Dafny compilation error occurred.
      if verificationResult.params == null then {
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
          println(s"No unverifiable claims could be found in the Dafny code for ${method.toString}.")
        } else {
          // TODO: Make compiler add relevant errors/warnings that IDEs can show (via report.error etc)
          println(s"Some claims in the Dafny code for ${method.toString} could not be verified.")
        }
      }

      // Close the "file"
      val didCloseMessage: String = DafnyLSPClient.constructLSPMessage("textDocument/didClose")(
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
      lspClient.sendMessage(didCloseMessage)
    }

    // Always return result of default runOn method for regular Scala compilation, as we do not modify it.
    result
  }
}
