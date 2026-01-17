package loreCompilerPlugin.codegen

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.report
import lore.ast.*
import lore.backends.{rename, traverseFromNode}
import loreCompilerPlugin.{DafnyEmbeddedLoReError, LogLevel}
import upickle.default.write as upickleWrite

import scala.annotation.unused
import scala.util.matching.Regex

/** Information about a definition in Dafny generated from a LoRe AST node.
  * @param name The name of the definition, equal across Dafny and LoRe
  * @param loreNode The original LoRe Node of this definition
  * @param loreType The type node of this definition in the LoRe AST
  * @param dafnyType The name of the type used in Dafny
  */
case class NodeInfo(
    name: String,
    loreNode: Term,
    loreType: Type,
    dafnyType: String
)

object DafnyGen {

  /** Takes a Scala type name and returns the corresponding Dafny type name, if one exists.
    * E.g. the corresponding Dafny type for the Scala type "String" is "string" (note the casing),
    * and the corresponding type for both the Float and Double types in Scala is the Dafny "real" type.
    * @param typeName The name of the Scala type.
    * @return The name of the corresponding Dafny type, or the original parameter if no such correspondence exists.
    */
  private def getDafnyType(typeName: String): String = {
    typeName match
        case "Boolean"          => "bool"
        case "Char"             => "char"
        case "Int"              => "int"
        case "Float" | "Double" => "real" // Dafny "real" follows SMT-Lib "Real" theory, so should be fine to map onto.
        case "String" => "string" // Technically seq<char> (String is Seq[Char]), syntactic sugar respectively.
        case "Map"    => "map"
        case "List"   => "seq"    // This does confer some changes in semantics regarding mutability etc.
        case _        => typeName
  }

  /** Recursively gathers the names of all references used in the given LoRe Term node.
    * This skips references to non-top-level definitions (e.g. references to parameters of arrow functions).
    * @param node The node to search.
    * @return The names of all references used in this node.
    */
  private def usedReferences(node: Term, ctx: Map[String, NodeInfo]): Set[String] = {
    // This uses sets to avoid duplicate entries.

    val refs: Set[String] = node match
        case t: (TViperImport | TArgT | TTypeAl | TNum | TTrue | TFalse | TString) => Set.empty // No references here
        case TVar(name, _, _)                                                      =>
          if !ctx.isDefinedAt(name) then Set.empty // Skip non-top-level definition references (e.g. arrow func params)
          else Set(name) // Regular reference to a top-level definition
        case TAbs(_, _, body, _, _)                                                => usedReferences(body, ctx)
        case TTuple(factors, _, _)         => factors.flatMap((n: Term) => usedReferences(n, ctx)).toSet
        case TIf(cond, _then, _else, _, _) =>
          val refs: Set[String] = usedReferences(cond, ctx) ++ usedReferences(_then, ctx)
          if _else.isDefined then refs ++ usedReferences(_else.get, ctx) else refs
        case TSeq(body, _, _) => body.toList.flatMap((n: Term) => usedReferences(n, ctx)).toSet
        case t: BinaryOp => usedReferences(t.left, ctx) ++ usedReferences(t.right, ctx) // Arith., Bool expr, Arrow func
        case TAssert(body, _, _)                   => usedReferences(body, ctx)
        case TAssume(body, _, _)                   => usedReferences(body, ctx)
        case t: TReactive                          => usedReferences(t.body, ctx)
        case TInteraction(_, _, m, r, e, ex, _, _) =>
          val reqs: Set[String] = r.flatMap((n: Term) => usedReferences(n, ctx)).toSet
          val ens: Set[String]  = e.flatMap((n: Term) => usedReferences(n, ctx)).toSet
          val refs: Set[String] = m.toSet ++ reqs ++ ens

          if ex.isDefined then refs ++ usedReferences(ex.get, ctx) else refs
        case TInvariant(condition, _, _) => usedReferences(condition, ctx)
        case TNeg(body, _, _)            => usedReferences(body, ctx)
        case t: TQuantifier              =>
          // vars are new definitions of TArgTs, they do not contain references, so skip those.
          // triggers should not contain any references that don't also appear in the body already, so skip too.
          usedReferences(t.body, ctx)
        case TParens(inner, _, _)          => usedReferences(inner, ctx)
        case TFCall(parent, _, args, _, _) =>
          val refs: Set[String] = usedReferences(parent, ctx)
          // The called field/method is not a standalone reference, so don't include it.
          // If this is a field call, args will be null, otherwise contains method parameters.
          args match
              case None    => refs
              case Some(a) => refs ++ a.flatMap((n: Term) => usedReferences(n, ctx)).toSet
        case TFCurly(parent, _, body, _, _) => usedReferences(parent, ctx) ++ usedReferences(body, ctx)
        case TFunC(_, args, _, _)           => args.flatMap((n: Term) => usedReferences(n, ctx)).toSet

    // Some LoRe types, which are defined as regular variables, are modelled differently in Dafny.
    // For example, Derived terms are modelled as functions in Dafny but regular variables in LoRe.
    // So whenever they are referenced in Dafny, they are not a variable reference, but a function call with parameters.
    // This means the reference list shouldn't include the plain name of that node, but instead its references within.
    // E.g. For foo = Derived { bar + someRef } with bar = Derived { otherRef + anotherRef }, the list of references for
    // "bar" is "otherRef" and "anotherRef", but the references for "foo" are the refs of "bar" and "someRef", i.e.
    // "otherRef", "anotherRef" and "someRef" instead of the refs being "bar" itself in addition to "someRef".
    // Therefore, these "deep" references have to be resolved down into their bare components and replaced in the list.
    val deepRefs: Set[String] =
      refs.filter { (ref: String) =>
        // This check is for skipping non-top-level definition references (e.g. arrow func parameters)
        if ctx.isDefinedAt(ref) then {
          val tp: Type = ctx(ref).loreType

          tp match
              case SimpleType(name, _) => name == "Signal" || name == "Interaction" || name == "Invariant"
              case TupleType(_)        =>
                // For a Derived/Interaction/Invariant to appear in a tuple type, it would have to be declared
                // within a tuple expression. However, this implementation forbids declarations for these types
                // to happen within other expressions, so this case is unreachable as long as that is unchanged.
                false
        } else false
      }

    if deepRefs.isEmpty then refs
    else {
      val deepRefsFlat: Set[String] = deepRefs.flatMap((ref: String) => usedReferences(ctx(ref).loreNode, ctx))

      // Remove direct Derived references (e.g. "foo" and "bar" above) and add their
      // flattened references (e.g. "someRef", "otherRef" and "anotherRef" in the above).
      refs -- deepRefs ++ deepRefsFlat
    }
  }

  /** Prepares the given pre-, postcondition or body term of an Interaction for Dafny code generation.
    * This includes replacing the names of arrow function arguments with the proper Source names specified
    * in the Interaction's modifies clause, as well as replacing references to Sources with field calls to
    * the main object's Source properties of the same names
    *
    * @param term          The Interaction term to prepare. These are always arrow functions.
    * @param modifiesNames The names of Sources specified in the Interaction's modifies clause.
    * @param argumentNames The names of arguments specified in the Interaction's executes clause.
    * @param sources       The list of info nodes on Sources defined in the program.
    * @return The equivalent term prepared for Dafny code generation.
    */
  private def prepareInteractionTerm(
      term: TArrow,
      modifiesNames: List[String],
      argumentNames: List[String],
      sources: List[NodeInfo]
  ): TArrow = {
    // Find list of currently used actual names in Interaction
    val args: List[String] = term.left match
        case TTuple(a, _, _) => a.collect {
            case TArgT(name, _, _, _) => name
          }
        case _ => List()
    val (actualReactiveNames, actualArgumentNames) = args.splitAt(modifiesNames.length)

    // Replace actual reactive names with those specified in the modifies clause
    val reactivesInserted: Term = actualReactiveNames.zip(modifiesNames).foldLeft(term.right) {
      case (body: Term, (from: String, to: String)) => rename(from, to, body)
    }

    // Replace actual argument names with those specified in the executes clause
    val argumentsInserted: Term = actualArgumentNames.zip(argumentNames).foldLeft(reactivesInserted) {
      case (body: Term, (from: String, to: String)) => rename(from, to, body)
    }

    // For each defined LoRe Source "source", replace e.g. "source + foo" with "LoreFields.source + foo".
    def replaceSourceRefWithFieldCall: Term => Term = {
      case t: TVar if sources.exists(node => node.name == t.name) =>
        TFCall(TVar("LoReFields"), t.name, None, t.sourcePos, t.scalaSourcePos)
      case t => t
    }

    // Replace Source references with field calls
    traverseFromNode(term.copy(right = argumentsInserted), replaceSourceRefWithFieldCall)
  }

  /** Creates a Dafny assignment statement from the given LoRe Interation executes term.
    * @param t The term to turn into an assignment (or multiple for tuples).
    * @param reactiveNames The names of the reactives specified in the Interaction's modifies clause.
    * @param ctx The compilation context for this Dafny code generation.
    * @return The assignment statement(s) for the Interaction's body.
    */
  private def getInteractionAssignment(t: Term, reactiveNames: List[String], ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    t match
        case tup: TTuple =>
          // Multiple Sources are being modified, so generate multiple assignments.
          if tup.factors.length != reactiveNames.length then {
            t.scalaSourcePos match
                case Some(pos) =>
                  report.error(
                    "Invalid executes in Interaction: " +
                    s"Number of modifies reactives (${reactiveNames.length}) " +
                    s"and number of values supplied in return value tuple (${tup.factors.length}) do not match.",
                    pos
                  )
                case None =>
                  report.error(
                    "Invalid executes in Interaction: " +
                    s"Number of modifies reactives (${reactiveNames.length}) " +
                    s"and number of values supplied in return value tuple (${tup.factors.length}) do not match."
                  )
          }

          // Order of assignments is the same as the order of Sources in the modifies list
          reactiveNames.zip(tup.factors)
            .map((r, v) => s"LoReFields.$r := ${generate(v, ctx)};")
            .mkString("\n")
        case _ =>
          // Body is a single expression for a single Source (i.e. not a tuple), so that becomes the assignment.
          s"LoReFields.${reactiveNames.head} := ${generate(t, ctx)};"
  }

  /** Compiles a list of LoRe terms into Dafny code.
    * @param ast The list of LoRe terms to compile to Dafny.
    * @return The generated Dafny code.
    */
  def generate(ast: List[Term], loreMethodName: String)(using logLevel: LogLevel, scalaCtx: Context): String = {
    // TODO: Simply using a map with a string and an associated node causes bugs because it ignores scoping.
    // Specifically, referencing definitions with the same name across different scoping levels will always be
    // assumed to reference the top-level definition instead, potentially causing errors about forward references
    // (if the top-level definition comes later) or being unable to reference e.g. reactives (if it comes before).
    var compilationContext: Map[String, NodeInfo] = Map()

    // Split term list into sublists that require different handling
    val termGroups: Map[String, List[Term]] = ast.groupBy {
      case TAbs(_, _type, _, _, _) =>
        _type match
            case SimpleType(name, _) =>
              if name == "Var" then "sourceDefs"
              else if name == "Signal" then "derivedDefs"
              else if name == "Interaction" then "interactionDefs"
              else if name == "Invariant" then "invariantDefs"
              else "otherDefs"
            case TupleType(_) =>
              // Reactives and Interactions are forbidden from being declared within tuples.
              // Therefore, a tuple type must always be a def that is neither of those.
              "otherDefs"
      case _ => "statements"
    }

    // Record compilation context info of all definitions (name, lore term, lore + dafny type) before generation
    if logLevel.isLevelOrHigher(LogLevel.Sparse) then println("Recording definitions in context...")
    termGroups.values.foreach { termList =>
      termList.foreach {
        case term @ TAbs(name, _type, body, _, _) =>
          val tp: String = generate(_type, compilationContext)
          compilationContext = compilationContext.updated(name, NodeInfo(name, term, _type, tp))
        case _ => ()
      }
    }

    // Generate Dafny code for all term groups
    // Also check for forward references in terms, and report an error for offending terms
    val dafnyCode: Map[String, List[String]] = termGroups.map { (termType, termList) =>
      if logLevel.isLevelOrHigher(LogLevel.Verbose) then println(s"Generating Dafny code for $termType...")

      val generated: List[String] = termList.map { term =>
        // Check for forward references
        val refs: Set[String] = usedReferences(term, compilationContext)
        refs.foreach { (ref: String) =>
          // Find the definition of the reference from context (should always be present because it was built above)
          val refDef: Option[NodeInfo] = compilationContext.get(ref)
          refDef match
              case None       => ()
              case Some(node) =>
                // If both terms have positions recorded, compare their starting line
                // If the ref term is ahead of (smaller than) the node def, this is a forward reference
                if term.scalaSourcePos.isDefined && node.loreNode.scalaSourcePos.isDefined then {
                  if term.scalaSourcePos.get.startLine < node.loreNode.scalaSourcePos.get.startLine then {
                    report.error("Forward references are not allowed.", term.scalaSourcePos.orNull)
                  }
                }
        }

        // Map to generation result of term
        generate(term, compilationContext)
      }

      if termType == "statements" then {
        // Statements end with a curly brace or a semicolon: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-statements
        // Therefore, if this statement doesn't end with a curly brace or semi already, it has to end with a semicolon.
        // This semicolon isn't already added in generation as it is not appropriate in all situations.
        // The curly brace however would always already have been added as part of always-required syntax.
        (termType, generated.filter(l => !l.isBlank).map(t => if t.endsWith("}") || t.endsWith(";") then t else s"$t;"))
      } else {
        // All other term types are taken as generated
        (termType, generated.filter(l => !l.isBlank))
      }
    }

    // Sources have to first be split into their components (name, type and value), since they're modelled as
    // Dafny class fields. Class fields can't be initialized immediately, only declared, and must be defined in a
    // constructor. The shape of the generated code for Sources is "var foo: bar := baz;", whereas declarations are
    // of the shape "var foo: bar" (no semicolon) and definitions are of the shape "foo := baz;" (with semicolon).
    // Additionally, ensures conditions are attached to the constructor for verifying their initial values.
    val sourceRegex: Regex                          = """var (.*): (.*) := (.*);""".r
    val sourceParts: List[(String, String, String)] = dafnyCode.getOrElse("sourceDefs", List()).map {
      case sourceRegex(name, _type, value) => (name, _type, value)
      case _                               => ("", "", "")
    }.filter((n, t, v) => !(n + t + v).isBlank)

    if logLevel.isLevelOrHigher(LogLevel.Sparse) then println("Generating composed Dafny code...")
    // Splice together generated Dafny code of all term groups appropriately.
    // Because the indent method adds a newline at the end of the string, the whole string has to be constructed
    // so that it looks as desired taking those newlines into account, particularly in the object constructor.
    // That is why the opening and closing brace aren't on their own lines already in the formatted string here.
    s"""// Generated from Scala: $loreMethodName
       |
       |// Main object containing all fields
       |class LoReFields {
       |  // Constant field definitions
       |${dafnyCode.getOrElse("otherDefs", List()).mkString("\n").indent(2)}
       |
       |  // Source declarations
       |${sourceParts.map((n, t, _) => s"var $n: $t").mkString("\n").indent(2)}
       |
       |  constructor ()
       |    // For verification purposes: Ensure sources are of the given initialization values.
       |${sourceParts.map((n, _, v) => s"ensures $n == $v").mkString("\n").indent(4)}
       |  {
       |    // Definition of Source values (initial values used in LoRe definition)
       |${sourceParts.map((n, _, v) => s"$n := $v;").mkString("\n").indent(4)}
       |  }
       |}
       |
       |// Derived definitions
       |${dafnyCode.getOrElse("derivedDefs", List()).mkString("\n|\n")}
       |
       |// Invariant definitions
       |${dafnyCode.getOrElse("invariantDefs", List()).mkString("\n|\n")}
       |
       |// Interaction definitions
       |${dafnyCode.getOrElse("interactionDefs", List()).mkString("\n|\n")}
       |
       |// Main method
       |method {:main} Main() {
       |  // Main object reference
       |  var LoReFields := new LoReFields();
       |
       |  // Statements: Function calls, method calls, if-conditions etc.
       |${dafnyCode.getOrElse("statements", List()).mkString("\n").indent(2)}
       |}""".linesIterator.filter(l => !l.isBlank).mkString("\n").stripMargin
  }

  /** Generates Dafny code for the given LoRe Term node.
    *
    * @param node The LoRe Term node.
    * @return The generated Dafny code.
    */
  private def generate(node: Term, ctx: Map[String, NodeInfo])(using logLevel: LogLevel, scalaCtx: Context): String = {
    node match
        // Cases ordered by order in LoRe AST definition.
        case n: TViperImport => generateFromTViperImport(n, ctx)
        case n: TArgT        => generateFromTArgT(n, ctx)
        case n: TVar         => generateFromTVar(n, ctx)
        case n: TAbs         => generateFromTAbs(n, ctx)
        case n: TTuple       => generateFromTTuple(n, ctx)
        case n: TIf          => generateFromTIf(n, ctx)
        case n: TSeq         => generateFromTSeq(n, ctx)
        case n: TArrow       => generateFromTArrow(n, ctx)
        case n: TTypeAl      => generateFromTTypeAl(n, ctx)
        case n: TAssert      => generateFromTAssert(n, ctx)
        case n: TAssume      => generateFromTAssume(n, ctx)
        case n: TReactive    => generateFromTReactive(n, ctx)
        case n: TInteraction => generateFromTInteraction(n, ctx)
        case n: TInvariant   => generateFromTInvariant(n, ctx)
        case n: TArith       => generateFromTArith(n, ctx)
        case n: TBoolean     => generateFromTBoolean(n, ctx)
        case n: TParens      => generateFromTParens(n, ctx)
        case n: TString      => generateFromTString(n, ctx)
        case n: TFAcc        => generateFromTFAcc(n, ctx)
        case n: TFunC        => generateFromTFunC(n, ctx)
  }

  /** Generates a Dafny type annotation for the given LoRe Type node.
    *
    * @param node The LoRe Type node.
    * @return The generated Dafny type annotation.
    */
  private def generate(node: Type, ctx: Map[String, NodeInfo])(using logLevel: LogLevel, scalaCtx: Context): String = {
    node match
        case n: SimpleType => generateFromSimpleType(n, ctx)
        case n: TupleType  => generateFromTupleType(n, ctx)
  }

  /** Generates a Dafny Type annotation for the given LoRe SimpleType node.
    *
    * @param node The LoRe SimpleType node.
    * @return The generated Dafny Type annotation.
    */
  private def generateFromSimpleType(node: SimpleType, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    val dafnyType: String = getDafnyType(node.name)

    // FYI: No special case for Interactions required as they're modelled as methods without return values.
    // Similarly, Invariants are generated as functions which always have the "bool" return type.

    if dafnyType == "Var" then {
      // Source (REScala Var) terms are modeled as Dafny fields typed after the inner type of the Source in LoRe.
      // That is to say, a "Source[Int]" is just an "int" type field in Dafny - the Source types does not appear.
      // A Source always only has one type parameter, so generate the annotation for it and return that value.
      generate(node.inner.head, ctx)
    } else if dafnyType == "Signal" then {
      // Derived (REScala Signal) terms are modeled as Dafny functions whose return type is the type parameter of the
      // Derived in LoRe. That is to say, a "foo: Derived[Int]" is a "function foo(...): int { ... }", with no mention
      // of a Derived type. A Derived only has one type parameter, so generate the annotation for it and return it.
      generate(node.inner.head, ctx)
    } else if dafnyType.matches("Function\\d+") then {
      // Anonymous functions

      // The input and output types for FunctionN types aren't split in its parameter list.
      // The name of the type however tells you the number of inputs, and there's always only one output in Scala/LoRe.
      // Therefore, grab the number from the type name and that many elements, and then the last element as output.
      val functionArity: Int   = dafnyType.split("Function").last.toInt
      val inputs: List[String] = node.inner.take(functionArity).map(p => generate(p, ctx))
      val output: String       = generate(node.inner.last, ctx)

      // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-arrow-types
      // TODO: Can be one of three arrow types: "->", "-->" or "~>".
      // Detecting which of the three would be appropriate depends on the function's body.
      // This is however out of scope for the current work, so use the type of all kinds of functions for now.
      s"(${inputs.mkString(", ")}) ~> $output"
    } else { // All other types are simply output according to regular Dafny type syntax
      val innerList: List[String] = node.inner.map(t => generate(t, ctx))
      val inner: String           = if innerList.isEmpty then "" else s"<${innerList.mkString(", ")}>"
      s"$dafnyType$inner"
    }
  }

  /** Generates a Dafny Type annotation for the given LoRe TupleType node.
    *
    * @param node The LoRe TupleType node.
    * @return The generated Dafny Type annotation.
    */
  private def generateFromTupleType(node: TupleType, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    val tupleElements: List[String] = node.inner.map(t => generate(t, ctx)).toList

    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-tuple-types
    s"(${tupleElements.mkString(", ")})"
  }

  /** Generates Dafny code for the given LoRe TArgT.
    *
    * @param node The LoRe TArgT node.
    * @return The generated Dafny code.
    */
  private def generateFromTArgT(node: TArgT, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    val typeAnnot: String = generate(node._type, ctx)

    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#273-identifier-type-combinations
    s"${node.name}: $typeAnnot"
  }

  /** Generates Dafny code for the given LoRe TVar.
    *
    * @param node The LoRe TVar node.
    * @return The generated Dafny code.
    */
  private def generateFromTVar(node: TVar, ctx: Map[String, NodeInfo])(using
      @unused logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    if !ctx.isDefinedAt(node.name) then return node.name // Skip non-top-level definition references

    ctx(node.name).loreType match
        case SimpleType(typeName, _) if typeName == "Signal" || typeName == "Var" =>
          // Reactives (i.e. Sources and Derived) may not be referenced plainly, only via the "value" method.
          // This is because Derived terms are translated into functions, and the type used for Derived in Dafny
          // is simply the type parameter of the Derived, or in Dafny's case the return type of the function.
          // However, when a Derived would be referenced without that reference being a call (which would turn
          // the reference's type into the return type), it would be necessary to give the reference the
          // _function type_ of that translated Derived instead. Therefore, when accessed, it must be via access
          // to the "value" property. For Sources this turns into a reference, for Deriveds into a function call.
          node.scalaSourcePos match
              case None =>
                report.error(
                  "Reactives may not be referenced directly, apart from calling the \"value\" or \"now\" property."
                )
              case Some(pos) =>
                report.error(
                  "Reactives may not be referenced directly, apart from calling the \"value\" or \"now\" property.",
                  pos
                )
          "<error>" // Still return a string value to satisfy compiler (this is invalid code but compilation fails anyway)
        case SimpleType("Interaction", _) =>
          // Interactions may not be referenced if said reference is not used to extend the Interaction with further
          // components (i.e. requires, ensures, executes, modifies), for similar reasons to Derived terms above.
          // References that extensions Interactions will already have been processed into TInteraction terms in the
          // rather than showing up as TVars, so references of such kind would not show up anymore in the LoRe AST.
          // Therefore, if a reference to an Interaction shows up, it is always faulty.
          node.scalaSourcePos match
              case None      => report.error("Interactions may not be referenced without further method calls.")
              case Some(pos) => report.error("Interactions may not be referenced without further method calls.", pos)
          "<error>" // Still return a string value to satisfy compiler (this is invalid code but compilation fails anyway)
        case SimpleType("Invariant", _) =>
          // Invariants may not be referenced, for mostly same reasons as Derived terms explained above.
          // Invariants however do not have the exception of access via the "value" method, as it does not exist.
          node.scalaSourcePos match
              case None      => report.error("Invariants may not be referenced.")
              case Some(pos) => report.error("Invariants may not be referenced.", pos)
          "<error>" // Still return a string value to satisfy compiler (this is invalid code but compilation fails anyway)
        case _ => node.name // Simply place the reference for other types
  }

  /** Generates Dafny code for the given LoRe TAbs.
    *
    * @param node The LoRe TAbs node.
    * @return The generated Dafny code.
    */
  private def generateFromTAbs(node: TAbs, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    if logLevel.isLevelOrHigher(LogLevel.Verbose) then println(s"Generating Dafny code for definition ${node.name}...")

    node.body match
        case n: TSource =>
          // Source terms are realized as Dafny fields that can be modified post-definition.
          // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-field-declaration
          s"var ${node.name}: ${generate(node._type, ctx)} := ${generate(n.body, ctx)};"
        case n: TDerived =>
          // Derived terms are realized as Dafny functions. Functions do not have side-effects.
          // The return type of these functions is the type parameter of the Derived, while any
          // references included in the body of the Derived are turned into function parameters.
          val references: Set[String] = usedReferences(n.body, ctx)
          val parameters: Set[String] = references.map(ref => s"$ref: ${ctx(ref).dafnyType}")

          // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-function-declaration
          s"""function ${node.name}(${parameters.mkString(", ")}): ${generate(node._type, ctx)} {
           |${generate(n.body, ctx).indent(2)}
           |}""".stripMargin
        case n: TInvariant =>
          // Invariant terms are also realized as Dafny functions, like Derived terms.
          // References are again turned into function parameters.
          // The body as well as return type of all Invariants is a boolean (expression).
          val references: Set[String] = usedReferences(n.condition, ctx)
          val parameters: Set[String] = references.map(ref => s"$ref: ${ctx(ref).dafnyType}")

          // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-function-declaration
          s"""function ${node.name}(${parameters.mkString(", ")}): bool {
           |${generate(n.condition, ctx).indent(2)}
           |}""".stripMargin
        case n: TInteraction =>
          // Interaction terms are realized as Dafny methods. Methods may have side-effects.
          // As Interactions modify state directly, they do not have return values, however.
          // Their parameters are the state (a reference to the main object) as well as
          // any parameters as specified in the definition of the Interaction.

          // Only generate Dafny code for fully-featured Interactions, i.e. those with both modifies and executes.
          // When either is lacking, the Interaction would be irrelevant either way, as it does not affect anything.
          // Additionally, incomplete Interactions lead to invalid generated Dafny code. Defining incomplete Interactions
          // in Scala and then completing them through a reference is fine, however.
          if n.modifies.isEmpty || n.executes.isEmpty then return ""

          val reactiveTypes: List[String] = n.reactiveType match
              case t: SimpleType    => List(generate(t, ctx))
              case TupleType(inner) => inner.map(t => generate(t, ctx)).toList
          val reactiveNames: List[String] = n.modifies

          val argumentTypes: List[String] = n.argumentType match
              case t: SimpleType    => List(generate(t, ctx))
              case TupleType(inner) => inner.map(t => generate(t, ctx)).toList
          val argumentNames: List[String] = n.executes match
              case None               => List() // No body in this Interaction
              case Some(term: TArrow) =>
                // Regular body: an arrow function

                // Find list of arguments by going through the left side.
                // Warning: Normally, this should be possible simply through accessing term.args, but that
                // property calls a function with similar behavior to the below, but it relies on TArrow nodes
                // being structured via currying rather than having a singular argument tuple in "left".
                val args: List[String] = term.left match
                    case TTuple(arguments, _, _) =>
                      // Index count is to swap in names for Scala args left blank
                      var argIdx: Int = 0

                      arguments.collect { arg =>
                        val argName: String = arg match
                            // Replace any arguments left as blanks in Scala with a name that's valid in Dafny
                            case TArgT(name, _, _, _) if name.startsWith("_") => s"arg$argIdx"
                            // Use given argument name if one as specified
                            case TArgT(name, _, _, _) => name
                            case _                    => s"arg$argIdx" // Should not happen, only so match is exhaustive

                        argIdx += 1
                        argName
                      }
                    case _ => List()

                // Reactives aren't arguments, so remove those
                args.drop(reactiveTypes.length)
              case Some(term) => List() // Irrelevant body: non-arrow func

          val definedSources: List[NodeInfo] = ctx.values.filter { definition =>
            definition.loreNode match
                case TAbs(name, _, body: TSource, _, _) => true
                case _                                  => false
          }.toList

          val preconditions: List[String] = n.requires.map {
            case t: TArrow =>
              t.right match
                  case s: TSeq =>
                    // Preconditions must be a single expression, so blocks are not allowed.
                    // This is because Dafny preconditions only allow a single expression.
                    s.scalaSourcePos match
                        case None      => report.error("Preconditions may only contain a single expression.")
                        case Some(pos) => report.error("Preconditions may only contain a single expression.", pos)
                    "<error>" // Compiler still requires a return value
                  case _ =>
                    val pre: TArrow = prepareInteractionTerm(t, reactiveNames, argumentNames, definedSources)
                    val embeddedError: DafnyEmbeddedLoReError = t.scalaSourcePos match
                        case None =>
                          DafnyEmbeddedLoReError(
                            "A precondition could be not be proven, but no Scala source position is available."
                          )
                        case Some(pos) =>
                          DafnyEmbeddedLoReError(
                            "This precondition could be not be proven.",
                            Some(pos.span.coords)
                          )
                    val embeddedErrorEsc: String = upickleWrite(embeddedError).replace("\"", "\\\"")

                    s"requires {:error \"$embeddedErrorEsc\"} ${generate(pre.right, ctx)}"
            case _ => ""
          }

          val postconditions: List[String] = n.ensures.map {
            case t: TArrow =>
              t.right match
                  case s: TSeq =>
                    // Postconditions must be a single expression, so blocks are not allowed.
                    // This is because Dafny postconditions only allow a single expression.
                    s.scalaSourcePos match
                        case None      => report.error("Postconditions may only contain a single expression.")
                        case Some(pos) => report.error("Postconditions may only contain a single expression.", pos)
                    "<error>" // Compiler still requires a return value
                  case _ =>
                    val post: TArrow = prepareInteractionTerm(t, reactiveNames, argumentNames, definedSources)
                    val embeddedError: DafnyEmbeddedLoReError = t.scalaSourcePos match
                        case None =>
                          DafnyEmbeddedLoReError(
                            "A postcondition could be not be proven, but no Scala source position is available."
                          )
                        case Some(pos) =>
                          DafnyEmbeddedLoReError(
                            "This postcondition could be not be proven.",
                            Some(pos.span.coords)
                          )
                    val embeddedErrorEsc: String = upickleWrite(embeddedError).replace("\"", "\\\"")

                    s"ensures {:error \"$embeddedErrorEsc\"} ${generate(post.right, ctx)}"
            case _ => ""
          }

          // As only objects can be referenced in Dafny modifies clauses, the main object is directly used here.
          // To still ensure only the specified Sources are modified, instead attach ensures clauses that include
          // a condition specifying the not-mentioned Sources are unmodified, i.e. "ensures old(obj.other) == obj.other"
          // for every Source that is not included in the modifies list, where obj is the main object of the program.
          val unmodifiedSources: List[NodeInfo] = definedSources.filter(node => !n.modifies.contains(node.name))
          val modifies: List[String]            = "modifies LoReFields" :: unmodifiedSources.map { source =>
            // No modifies-specific position is available, since the modifies list isn't a list of terms, but strings
            val embeddedError: DafnyEmbeddedLoReError = n.scalaSourcePos match
                case None =>
                  DafnyEmbeddedLoReError(
                    s"It could not be proven that the ${source.name} Source, which was not specified in this Interaction's modifies clause, was not modified."
                  )
                case Some(pos) =>
                  DafnyEmbeddedLoReError(
                    s"It could not be proven that the ${source.name} Source, which was not specified in this Interaction's modifies clause, was not modified.",
                    Some(pos.span.coords)
                  )
            val embeddedErrorEsc: String = upickleWrite(embeddedError).replace("\"", "\\\"")

            s"ensures {:error \"$embeddedErrorEsc\"} old(LoReFields.${source.name}) == LoReFields.${source.name}"
          }

          val definedInvariants: List[NodeInfo] = ctx.values.filter { definition =>
            definition.loreNode match
                case TAbs(name, _, body: TInvariant, _, _) => true
                case _                                     => false
          }.toList

          // Get any Invariants which includes references to any of the Sources modified by this Interaction
          val relevantInvariants: List[NodeInfo] = definedInvariants.filter { inv =>
            inv.loreNode match
                case TAbs(_, _, TInvariant(cond, _, _), _, _) =>
                  val invRefs: Set[String] = usedReferences(cond, ctx)
                  reactiveNames.exists(s => invRefs.contains(s))
                case _ => false
          }

          val invariants: List[String] = relevantInvariants.map { inv =>
            // No Invariant-specific position is available, since the Invariants aren't specifically
            // called for individual Interactions, but checked through other means in Scala execution.
            val (embeddedErrorPre, embeddedErrorPost): (DafnyEmbeddedLoReError, DafnyEmbeddedLoReError) =
              n.scalaSourcePos match
                  case None =>
                    val preErr: DafnyEmbeddedLoReError = DafnyEmbeddedLoReError(
                      s"Prior to execution of this Interaction, the ${inv.name} Invariant could not be proven to hold."
                    )
                    val postErr: DafnyEmbeddedLoReError = DafnyEmbeddedLoReError(
                      s"After execution of this Interaction, the ${inv.name} Invariant could not be proven to hold."
                    )
                    (preErr, postErr)
                  case Some(pos) =>
                    val preErr: DafnyEmbeddedLoReError = DafnyEmbeddedLoReError(
                      s"Prior to execution of this Interaction, the ${inv.name} Invariant could not be proven to hold.",
                      Some(pos.span.coords)
                    )
                    val postErr: DafnyEmbeddedLoReError = DafnyEmbeddedLoReError(
                      s"After execution of this Interaction, the ${inv.name} Invariant could not be proven to hold.",
                      Some(pos.span.coords)
                    )
                    (preErr, postErr)

            val embeddedErrorsEsc: (String, String) = (
              upickleWrite(embeddedErrorPre).replace("\"", "\\\""),
              upickleWrite(embeddedErrorPost).replace("\"", "\\\"")
            )

            // Assemble list of parameters for Invariant call:
            // Any calls to definitions that exist are turned into field calls on the main object.
            val refs: Set[String] = usedReferences(inv.loreNode, ctx).map { ref =>
              if ctx.exists((name, node) => name == ref) then s"LoReFields.$ref" else ref
            }

            // Call the Invariant both before execution and after execution of the Interaction
            s"""requires {:error \"${embeddedErrorsEsc._1}\"} ${inv.name}(${refs.mkString(", ")})
             |ensures {:error \"${embeddedErrorsEsc._2}\"} ${inv.name}(${refs.mkString(", ")})""".stripMargin
          }

          // In Dafny, unlike Scala/LoRe, the return value of methods isn't decided by whatever the last expression in the
          // block is. Instead, return values are named and appear similar to local variables, and must be assigned to in
          // the body of the method at some point (as often as desired, even). Then, at the end of the method's body, the
          // current value of those return value variables is taken as the method's return value(s). The methods which are
          // used to model Interactions don't have any return values to begin with, so because in Scala/LoRe the last
          // expression decides the return value that will be assigned to the Source that is being modifies (as specified
          // in the modifies clause), all that needs to be done is get the last expression of the executes block
          // specified in LoRe, and then turn that into an assignment to the Source specified in the modifies clause.
          // This last expression already must be of the correct type, otherwise the Scala type checker would've errored.
          // If the Interaction modifies multiple Sources, this expression will be a tuple in Scala. Therefore, create
          // an assignment in Dafny for each item of the tuple to the specified modifies-Sources in order.
          val body: String = n.executes match
              case Some(t: TArrow) =>
                val bodyArrow: TArrow = prepareInteractionTerm(t, reactiveNames, argumentNames, definedSources)
                bodyArrow.right match
                    case seq: TSeq =>
                      // Body is a block, so go through and turn the last expression into Source assignment(s).
                      val bodyStatements: List[String] =
                        seq.body.take(seq.body.length - 1).map(term => generate(term, ctx))
                      val bodyAssignment: String = getInteractionAssignment(seq.body.last, reactiveNames, ctx)

                      // All statements included in the method body have to end with either a closing brace or a semicolon
                      (bodyStatements :+ bodyAssignment).map { l =>
                        if l.endsWith("}") || l.endsWith(";") then l else s"$l;"
                      }.mkString("\n")
                    case b: Term =>
                      // Body is a single expression, so turn that into the assignment(s).
                      getInteractionAssignment(b, reactiveNames, ctx)
              case Some(t: Term) => "" // Non-arrow function bodies are irrelevant
              case None          => "" // No body specified

          // Arguments for the Interaction are the main object (for Sources), as well as any non-Source executes arguments
          val argsString: String = argumentNames.zip(argumentTypes) match
              case Nil  => "LoReFields: LoReFields"
              case args => s"LoReFields: LoReFields, ${args.map((name, tp) => s"$name: $tp").mkString(", ")}"

          // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-method-declaration
          s"""method ${node.name}($argsString)
           |${preconditions.mkString("\n").indent(2)}
           |${postconditions.mkString("\n").indent(2)}
           |${modifies.mkString("\n").indent(2)}
           |${invariants.mkString("\n").indent(2)}
           |{
           |${body.indent(2)}
           |}""".stripMargin.linesIterator.filter(l => !l.isBlank).mkString("\n")
        case _ =>
          // Default into generic *const* declarations for other types, as all TAbs are by default non-mutable.
          val typeAnnot: String = generate(node._type, ctx)
          val body: String      = generate(node.body, ctx)
          s"const ${node.name}: $typeAnnot := $body"
  }

  /** Generates Dafny code for the given LoRe TTuple.
    *
    * @param node The LoRe TTuple node.
    * @return The generated Dafny code.
    */
  private def generateFromTTuple(node: TTuple, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    val elems: List[String] = node.factors.map(t => generate(t, ctx))

    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-tuple-types
    s"(${elems.mkString(", ")})"
  }

  /** Generates Dafny code for the given LoRe TIf.
    *
    * @param node The LoRe TIf node.
    * @return The generated Dafny code.
    */
  private def generateFromTIf(node: TIf, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    val cond: String     = generate(node.cond, ctx)
    val thenExpr: String = generate(node._then, ctx)
    val elseExpr: String = if node._else.isDefined then generate(node._else.get, ctx) else ""

    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-if-statement
    if elseExpr.isEmpty then {
      s"""if $cond {
         |${(if thenExpr.endsWith("}") || thenExpr.endsWith(";") then thenExpr else s"$thenExpr;").indent(2)}
         |}""".stripMargin
    } else {
      s"""if $cond {
         |${(if thenExpr.endsWith("}") || thenExpr.endsWith(";") then thenExpr else s"$thenExpr;").indent(2)}
         |} else {
         |${(if elseExpr.endsWith("}") || elseExpr.endsWith(";") then elseExpr else s"$elseExpr;").indent(2)}
         |}""".stripMargin
    }
  }

  /** Generates Dafny code for the given LoRe TSeq.
    *
    * @param node The LoRe TSeq node.
    * @return The generated Dafny code.
    */
  private def generateFromTSeq(node: TSeq, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    // TSeq terms are not "sequences" as in collections of values, they're blocks of statements.
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-block-statement
    node.body.map { t =>
      val gen: String = generate(t, ctx)
      // Statements end with a curly brace or a semicolon: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-statements
      // Therefore, if this statement doesn't end with a curly brace or semi already, it has to end with a semicolon.
      // This semicolon isn't already added in generation as it is not appropriate in all situations.
      // The curly brace however would always already have been added as part of always-required syntax.
      if gen.endsWith("}") || gen.endsWith(";") then gen else s"$gen;"
    }.toList.mkString("\n")
  }

  /** Generates Dafny code for the given LoRe TArrow.
    *
    * @param node The LoRe TArrow node.
    * @return The generated Dafny code.
    */
  private def generateFromTArrow(node: TArrow, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    val arrowHead: String = generate(node.left, ctx)
    val arrowBody: String = generate(node.right, ctx)

    // The anonymous function body always uses "=>" in Dafny.
    // Differentiation between "->", "-->" and "~>" is for types.
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-lambda-expression
    s"$arrowHead => $arrowBody"
  }

  /** Generates Dafny code for the given LoRe TAssert.
    *
    * @param node The LoRe TAssert node.
    * @return The generated Dafny code.
    */
  private def generateFromTAssert(node: TAssert, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-assert-statement
    s"assert ${generate(node.body, ctx)}"

  /** Generates Dafny code for the given LoRe TAssume.
    *
    * @param node The LoRe TAssume node.
    * @return The generated Dafny code.
    */
  private def generateFromTAssume(node: TAssume, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-assume-statement
    s"assume ${generate(node.body, ctx)}"

  /** Generates Dafny code for the given LoRe TReactive.
    *
    * @param node The LoRe TReactive node.
    * @return The generated Dafny code.
    */
  private def generateFromTReactive(node: TReactive, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    // These methods would only be entered if a reactive was declared as a literal within another expression.
    // These use-cases are not supported, which is why the below methods simply report a compiler error instead.
    // For the actual generation, such as the function modelling of Derived, see the generateFromTAbs function.
    val reactive: String = node match
        case n: TSource  => generateFromTSource(n, ctx)
        case n: TDerived => generateFromTDerived(n, ctx)

    reactive
  }

  /** Generates Dafny code for the given LoRe TSource.
    *
    * @param node The LoRe TSource node.
    * @return The generated Dafny code.
    */
  private def generateFromTSource(node: TSource, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    // Source terms are realized as Dafny fields, which happens in generateFromTAbs. This method is only entered
    // when a Source term is declared as a literal within another expression - a use-case that is not supported.
    node.scalaSourcePos match
        case Some(pos) => report.error("Declaring Source reactives within other expressions is not supported.", pos)
        case None      => report.error("Declaring Source reactives within other expressions is not supported.")
    "<error>"
  }

  /** Generates Dafny code for the given LoRe TDerived.
    *
    * @param node The LoRe TDerived node.
    * @return The generated Dafny code.
    */
  private def generateFromTDerived(node: TDerived, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    // Derived terms are realized as Dafny functions, which happens in generateFromTAbs. This method is only entered
    // when a Derived term is declared as a literal within another expression - a use-case that is not supported.
    node.scalaSourcePos match
        case Some(pos) => report.error("Declaring Derived reactives within other expressions is not supported.", pos)
        case None      => report.error("Declaring Derived reactives within other expressions is not supported.")
    "<error>"
  }

  /** Generates Dafny code for the given LoRe TInteraction.
    *
    * @param node The LoRe TInteraction node.
    * @return The generated Dafny code.
    */
  private def generateFromTInteraction(node: TInteraction, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    // Interaction terms are realized as Dafny methods, which happens in generateFromTAbs. This method is only entered
    // when an Interaction term is declared as a literal within another expression - a use-case that is not supported.
    node.scalaSourcePos match
        case Some(pos) => report.error("Declaring Interactions within other expressions is not supported.", pos)
        case None      => report.error("Declaring Interactions within other expressions is not supported.")
    "<error>"
  }

  /** Generates Dafny code for the given LoRe TInvariant.
    *
    * @param node The LoRe TInvariant node.
    * @return The generated Dafny code.
    */
  private def generateFromTInvariant(node: TInvariant, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    // Invariant terms are realized as Dafny functions, like Derived terms, and this happens in generateFromTAbs.
    // This method is only entered when an Invariant term is declared as a literal within another expression.
    // Said use-case is not supported.
    node.scalaSourcePos match
        case Some(pos) => report.error("Declaring Invariants within other expressions is not supported.", pos)
        case None      => report.error("Declaring Invariants within other expressions is not supported.")
    "<error>"
  }

  /** Generates Dafny code for the given LoRe TArith.
    *
    * @param node The LoRe TArith node.
    * @return The generated Dafny code.
    */
  private def generateFromTArith(node: TArith, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    // FYI: Modulo and Unary Minus do not exist in LoRe, but do in Dafny.
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-numeric-types
    val expr: String = node match
        case n: TNum => generateFromTNum(n, ctx)
        case n: TDiv => generateFromTDiv(n, ctx)
        case n: TMul => generateFromTMul(n, ctx)
        case n: TAdd => generateFromTAdd(n, ctx)
        case n: TSub => generateFromTSub(n, ctx)

    node match
        case n: TNum => expr // Simple numbers don't need parens as there is no nesting at this level.
        case _       =>
          s"($expr)" // Surround with parens to respect expression nesting as instructed by the AST node nesting.
  }

  /** Generates Dafny code for the given LoRe TNum.
    *
    * @param node The LoRe TNum node.
    * @return The generated Dafny code.
    */
  private def generateFromTNum(node: TNum, @unused ctx: Map[String, NodeInfo])(using
      @unused logLevel: LogLevel,
      @unused scalaCtx: Context
  ): String =
    // Transforming an integer into a string to output a number may seem odd,
    // but in reality it'll be a number in code as it's not surrounded by quotes.
    node.value.toString

  /** Generates Dafny code for the given LoRe TDiv.
    *
    * @param node The LoRe TDiv node.
    * @return The generated Dafny code.
    */
  private def generateFromTDiv(node: TDiv, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    s"${generate(node.left, ctx)} / ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TMul.
    *
    * @param node The LoRe TMul node.
    * @return The generated Dafny code.
    */
  private def generateFromTMul(node: TMul, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    s"${generate(node.left, ctx)} * ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TAdd.
    *
    * @param node The LoRe TAdd node.
    * @return The generated Dafny code.
    */
  private def generateFromTAdd(node: TAdd, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    s"${generate(node.left, ctx)} + ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TSub.
    *
    * @param node The LoRe TSub node.
    * @return The generated Dafny code.
    */
  private def generateFromTSub(node: TSub, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    s"${generate(node.left, ctx)} - ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TBoolean.
    *
    * @param node The LoRe TBoolean node.
    * @return The generated Dafny code.
    */
  private def generateFromTBoolean(node: TBoolean, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-booleans
    val expr: String = node match
        case n: TTrue       => generateFromTTrue(n, ctx)
        case n: TFalse      => generateFromTFalse(n, ctx)
        case n: TNeg        => generateFromTNeg(n, ctx)
        case n: TLt         => generateFromTLt(n, ctx)
        case n: TGt         => generateFromTGt(n, ctx)
        case n: TLeq        => generateFromTLeq(n, ctx)
        case n: TGeq        => generateFromTGeq(n, ctx)
        case n: TEq         => generateFromTEq(n, ctx)
        case n: TIneq       => generateFromTIneq(n, ctx)
        case n: TDisj       => generateFromTDisj(n, ctx)
        case n: TConj       => generateFromTConj(n, ctx)
        case n: TImpl       => generateFromTImpl(n, ctx)
        case n: TBImpl      => generateFromTBImpl(n, ctx)
        case n: TInSet      => generateFromTInSet(n, ctx)
        case n: TQuantifier => generateFromTQuantifier(n, ctx)

    node match
        case n: (TTrue | TFalse) => expr // Simple booleans don't need parens because there is no nesting at this level.
        case _                   =>
          s"($expr)" // Surround with parens to respect expression nesting as instructed by the AST node nesting.
  }

  /** Generates Dafny code for the given LoRe TTrue.
    *
    * @param node The LoRe TTrue node.
    * @return The generated Dafny code.
    */
  private def generateFromTTrue(node: TTrue, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    "true"

  /** Generates Dafny code for the given LoRe TFalse.
    *
    * @param node The LoRe TFalse node.
    * @return The generated Dafny code.
    */
  private def generateFromTFalse(node: TFalse, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    "false"

  /** Generates Dafny code for the given LoRe TNeg.
    *
    * @param node The LoRe TNeg node.
    * @return The generated Dafny code.
    */
  private def generateFromTNeg(node: TNeg, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    s"!${generate(node.body, ctx)}"

  /** Generates Dafny code for the given LoRe TLt.
    *
    * @param node The LoRe TLt node.
    * @return The generated Dafny code.
    */
  private def generateFromTLt(node: TLt, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    s"${generate(node.left, ctx)} < ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TGt.
    *
    * @param node The LoRe TGt node.
    * @return The generated Dafny code.
    */
  private def generateFromTGt(node: TGt, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    s"${generate(node.left, ctx)} > ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TLeq.
    *
    * @param node The LoRe TLeq node.
    * @return The generated Dafny code.
    */
  private def generateFromTLeq(node: TLeq, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    s"${generate(node.left, ctx)} <= ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TGeq.
    *
    * @param node The LoRe TGeq node.
    * @return The generated Dafny code.
    */
  private def generateFromTGeq(node: TGeq, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    s"${generate(node.left, ctx)} >= ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TEq.
    *
    * @param node The LoRe TEq node.
    * @return The generated Dafny code.
    */
  private def generateFromTEq(node: TEq, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    s"${generate(node.left, ctx)} == ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TIneq.
    *
    * @param node The LoRe TIneq node.
    * @return The generated Dafny code.
    */
  private def generateFromTIneq(node: TIneq, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    s"${generate(node.left, ctx)} != ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TDisj.
    *
    * @param node The LoRe TDisj node.
    * @return The generated Dafny code.
    */
  private def generateFromTDisj(node: TDisj, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    s"${generate(node.left, ctx)} || ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TConj.
    *
    * @param node The LoRe TConj node.
    * @return The generated Dafny code.
    */
  private def generateFromTConj(node: TConj, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    s"${generate(node.left, ctx)} && ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TImpl.
    *
    * @param node The LoRe TImpl node.
    * @return The generated Dafny code.
    */
  private def generateFromTImpl(node: TImpl, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    // FYI: Dafny also supports a "reverse implication", i.e. right implies left, but this doesn't exist in LoRe.
    s"${generate(node.left, ctx)} ==> ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TBImpl.
    *
    * @param node The LoRe TBImpl node.
    * @return The generated Dafny code.
    */
  private def generateFromTBImpl(node: TBImpl, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    s"${generate(node.left, ctx)} <==> ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TInSet.
    *
    * @param node The LoRe TInSet node.
    * @return The generated Dafny code.
    */
  private def generateFromTInSet(node: TInSet, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    // FYI: Dafny has a syntactic shorthand for in-set negation: "x !in y". This does not exist in LoRe.
    s"${generate(node.left, ctx)} in ${generate(node.right, ctx)}"

  /** Generates Dafny code for the given LoRe TQuantifier.
    *
    * @param node The LoRe TQuantifier node.
    * @return The generated Dafny code.
    */
  private def generateFromTQuantifier(node: TQuantifier, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-quantifier-expression
    val expr: String = node match
        case n: TForall => generateFromTForall(n, ctx)
        case n: TExists => generateFromTExists(n, ctx)

    s"($expr)" // Surround with parens to respect expression nesting as instructed by the AST node nesting.
  }

  /** Generates Dafny code for the given LoRe TParens.
    *
    * @param node The LoRe TParens node.
    * @return The generated Dafny code.
    */
  private def generateFromTParens(node: TParens, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    // This node simply surrounds the contained expression with parens.
    s"(${generate(node.inner, ctx)})"

  /** Generates Dafny code for the given LoRe TString.
    *
    * @param node The LoRe TString node.
    * @return The generated Dafny code.
    */
  private def generateFromTString(node: TString, @unused ctx: Map[String, NodeInfo])(using
      @unused logLevel: LogLevel,
      @unused scalaCtx: Context
  ): String =
    // Surround by quotes so it's an actual string within the resulting Dafny code.
    // Could technically also be output as a sequence of chars, if this was desired.
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-strings
    s"\"${node.value}\""

  /** Generates Dafny code for the given LoRe TFAcc.
    *
    * @param node The LoRe TFAcc node.
    * @return The generated Dafny code.
    */
  private def generateFromTFAcc(node: TFAcc, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    val fieldAccess: String = node match
        case n: TFCall  => generateFromTFCall(n, ctx)
        case n: TFCurly => generateFromTFCurly(n, ctx)

    fieldAccess
  }

  /** Generates Dafny code for the given LoRe TFCall.
    *
    * @param node The LoRe TFCall node.
    * @return The generated Dafny code.
    */
  private def generateFromTFCall(node: TFCall, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    node.args match
        case None =>
          // Property (field) access

          // References:
          // https://dafny.org/dafny/DafnyRef/DafnyRef#sec-field-declaration
          // https://dafny.org/dafny/DafnyRef/DafnyRef#sec-class-types

          // Accesses to the "value"/"now" field of a Source/Derived reference in LoRe are modeled differently in Dafny.
          // For Sources, it simply represents a field access to the Source. For Derived, it's a call to the function.
          // Therefore, the call to the "value"/"now" property has to be replaced by the respective ref or function call.
          node.parent match
              case n: TVar if node.field == "value" || node.field == "now" =>
                val refType: Type = ctx(n.name).loreType

                refType match
                    case SimpleType(name, _) if name == "Var" => // Source is REScala "Var" type
                      n.name
                    case SimpleType(name, _) if name == "Signal" => // Derived is REScala "Signal" type
                      val refs: Set[String] = usedReferences(node.parent, ctx)
                      s"${n.name}(${refs.mkString(", ")})"
                    case _ => // "value" property access of a non-Source/non-Derived
                      s"${generate(node.parent, ctx)}.${node.field}"
              case _ => // Any other field accesses
                s"${generate(node.parent, ctx)}.${node.field}"
        case Some(args) =>
          // Method access
          // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-method-declaration
          val argsList: List[String] = args.map(arg => generate(arg, ctx))
          s"${generate(node.parent, ctx)}.${node.field}(${argsList.mkString(", ")})"
  }

  /** Generates Dafny code for the given LoRe TFunC.
    *
    * @param node The LoRe TFunc node.
    * @return The generated Dafny code.
    */
  private def generateFromTFunC(node: TFunC, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String = {
    // References:
    // https://dafny.org/dafny/DafnyRef/DafnyRef#sec-function-declaration
    // https://dafny.org/dafny/DafnyRef/DafnyRef#sec-maps
    // https://dafny.org/dafny/DafnyRef/DafnyRef#sec-sequences

    node.name match
        case "Map" =>
          // Map instantiations differ from regular function calls.
          // Each map pair is a 2-tuple (i.e. length 2 TTuple in LoRe).
          val mapKeyValues: Seq[String] = node.args.map { kv =>
            // Simply throwing these TTuples to generate would give us tuple syntax, not map syntax.
            // Therefore, generate key and value separately and combine them with appropriate Dafny syntax.
            val keyValueTuple: TTuple = kv.asInstanceOf[TTuple]
            val key: String           = generate(keyValueTuple.factors.head, ctx)
            val value: String         = generate(keyValueTuple.factors.last, ctx)
            s"$key := $value"
          }
          s"map[${mapKeyValues.mkString(", ")}]"
        case "List" =>
          // List instantiations also differ, these are turned into Dafny sequences.
          val items: Seq[String] = node.args.map(i => generate(i, ctx))
          s"[${items.mkString(", ")}]"
        case "println" =>
          // Dafny does not have a "println" function, so change the name of the function being called to "print", which
          // does exist in Dafny, and append a second parameter to the parameter list, that being the newline character.
          val printlnCall: TFunC = node.copy(
            name = "print",            // Change function name from "println" to "print"
            args = node.args.appended( // Add on a "\n" as last parameter for the print call
              if node.args.isEmpty
              then TString("\\n", node.sourcePos, node.scalaSourcePos)
              else TString("\\n", node.args.last.sourcePos, node.args.last.scalaSourcePos)
            )
          )

          val args: Seq[String] = printlnCall.args.map(arg => generate(arg, ctx))
          // The Dafny "print" function doesn't use parens when multiple parameters are supplied
          s"${printlnCall.name} ${args.mkString(", ")}"
        case _ =>
          val args: Seq[String] = node.args.map(arg => generate(arg, ctx))
          s"${node.name}(${args.mkString(", ")})"
  }

  /* Term types that are not covered currently, and should error */

  /** Generates Dafny code for the given LoRe TViperImport.
    *
    * @param node The LoRe TViperImport node.
    * @return The generated Dafny code.
    */
  private def generateFromTViperImport(node: TViperImport, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    // Viper-specific (by name at least). Leave out for now, maybe reused for Dafny imports in later work.
    throw new Error("Term type not implemented")

  /** Generates Dafny code for the given LoRe TTypeAl.
    *
    * @param node The LoRe TTypeAl node.
    * @return The generated Dafny code.
    */
  private def generateFromTTypeAl(node: TTypeAl, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    // Leave out for now. Maybe in later work.
    throw new Error("Term type not implemented")

  /** Generates Dafny code for the given LoRe TForall.
    *
    * @param node The LoRe TForall node.
    * @return The generated Dafny code.
    */
  private def generateFromTForall(node: TForall, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    // To be implemented later, but out of scope for the current project.
    throw new Error("Term type not implemented")

  /** Generates Dafny code for the given LoRe TExists.
    *
    * @param node The LoRe TExists node.
    * @return The generated Dafny code.
    */
  private def generateFromTExists(node: TExists, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    // To be implemented later, but out of scope for the current project.
    throw new Error("Term type not implemented")

  /** Generates Dafny code for the given LoRe TFCurly.
    *
    * @param node The LoRe TFCurly node.
    * @return The generated Dafny code.
    */
  private def generateFromTFCurly(node: TFCurly, ctx: Map[String, NodeInfo])(using
      logLevel: LogLevel,
      scalaCtx: Context
  ): String =
    // Probably not needed for Dafny. Leave out for now, maybe revisited in later work.
    throw new Error("Term type not implemented")
}
