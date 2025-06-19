package loreCompilerPlugin.codegen

import cats.data.NonEmptyList
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Types.{AppliedType, CachedTypeRef, TypeRef, Type as ScalaType}
import dotty.tools.dotc.report
import dotty.tools.dotc.util.SourcePosition
import lore.ast.{Type as LoReType, *}
import loreCompilerPlugin.LogLevel

import scala.annotation.unused

object LoReGen {

  /** Logs info about a RHS value. Not very robust, rather a (temporary?) solution to prevent large logging code duplication.
    *
    * @param indentLevel How many tabs should be placed before the text that will be logged
    * @param operandSide Additional text to indicate more information about the parameter (e.g. "left" will result in "left parameter")
    * @param rhsType     The type of the RHS in question
    * @param rhsValue    The value of the RHS in question
    */
  private def logRhsInfo(indentLevel: Integer, operandSide: String, rhsType: String, rhsValue: String)(using
      @unused logLevel: LogLevel
  ): Unit = {
    if operandSide.nonEmpty then {
      println(s"${"\t".repeat(indentLevel)}The $operandSide parameter is a $rhsType $rhsValue")
    } else {
      println(s"${"\t".repeat(indentLevel)}The parameter is a $rhsType $rhsValue")
    }
  }

  /** Creates a LoRe Term from a Scala Tree.
    *
    * @param tree The Scala Tree.
    * @param termList The list of already-created LoRe terms for this program.
    * @return The LoRe Term.
    */
  def createLoReTermFromTree(tree: tpd.Tree, termList: List[Term])(using
      logLevel: LogLevel,
      ctx: Context
  ): List[Term] = {
    // Returns a List instead of a singular term because of blocks
    tree match
      case ap: Apply[?] => // Function or method calls, e.g. "println(...)" or "foo.bar()"
        List(createLoReTermFromApply(ap, termList))
      case bl: Block[?] => // Blocks of trees
        val blockTerms: List[Term] = bl.stats.flatMap(t => createLoReTermFromTree(t, termList))

        // All of the block's trees, apart from the very last (which is used as return value), are included
        // in the "stats" property. We also want the last one however, so add it to the list manually.
        // The exception to this is when the last tree is a definition, where expr is an Constant Literal of Unit.
        bl.expr match
          case Literal(Constant(_: Unit)) => blockTerms
          case _                          => blockTerms :++ createLoReTermFromTree(bl.expr, termList)
      case id: Ident[?] => // References
        List(createLoReTermFromIdent(id, termList))
      case i: If[?] => // If definitions, i.e. "if foo then bar else baz" where foo is a boolean expr
        List(createLoReTermFromIf(i, termList))
      case li: Literal[?] => // Literals (e.g. 0, "foo", ...)
        List(createLoReTermFromLiteral(li, termList))
      case se: Select[?] => // Property access, e.g. "foo.bar"
        List(createLoReTermFromSelect(se, termList))
      case vd: ValDef[?] => // Val definitions, i.e. "val foo: Bar = baz" where baz is any valid RHS
        List(createLoReTermFromValDef(vd, termList))
      // Implement other Tree types for the frontend here
      case _ =>
        report.error(s"This syntax is not supported in LoRe.", tree.sourcePos)
        List(TVar("<error>")) // Make compiler happy
  }

  /** Creates a LoRe Term from a Scala Apply Tree.
    *
    * @param tree The Scala Apply Tree.
    * @param termList The list of already-created LoRe terms for this program.
    * @return The LoRe Term.
    */
  private def createLoReTermFromApply(tree: tpd.Apply, termList: List[Term])(using
      logLevel: LogLevel,
      ctx: Context
  ): Term = {
    // Apply statements are covered as part of RHS term building for ValDefs
    buildLoReRhsTerm(tree, termList)
  }

  /** Creates a LoRe Term from a Scala Ident Tree.
    *
    * @param tree The Scala Ident Tree.
    * @param termList The list of already-created LoRe terms for this program.
    * @return The LoRe Term.
    */
  private def createLoReTermFromIdent(tree: tpd.Ident, termList: List[Term])(using
      logLevel: LogLevel,
      ctx: Context
  ): Term = {
    // Ident statements are covered as part of RHS term building for ValDefs
    buildLoReRhsTerm(tree, termList)
  }

  /** Creates a LoRe Term from a Scala If Tree.
    *
    * @param tree     The Scala If Tree.
    * @param termList The list of already-created LoRe terms for this program.
    * @return The LoRe Term.
    */
  private def createLoReTermFromIf(tree: tpd.If, termList: List[Term])(using
      logLevel: LogLevel,
      ctx: Context
  ): Term = {
    // If statements are covered as part of RHS term building for ValDefs
    buildLoReRhsTerm(tree, termList)
  }

  /** Creates a LoRe Term from a Scala Literal Tree.
    *
    * @param tree The Scala Literal Tree.
    * @param termList The list of already-created LoRe terms for this program.
    * @return The LoRe Term.
    */
  private def createLoReTermFromLiteral(tree: tpd.Literal, termList: List[Term])(using
      logLevel: LogLevel,
      ctx: Context
  ): Term = {
    // Literal statements are covered as part of RHS term building for ValDefs
    buildLoReRhsTerm(tree, termList)
  }

  /** Creates a LoRe Term from a Scala Select Tree.
    *
    * @param tree The Scala Select Tree.
    * @param termList The list of already-created LoRe terms for this program.
    * @return The LoRe Term.
    */
  private def createLoReTermFromSelect(tree: tpd.Select, termList: List[Term])(using
      logLevel: LogLevel,
      ctx: Context
  ): Term = {
    // Select statements are covered as part of RHS term building for ValDefs
    buildLoReRhsTerm(tree, termList)
  }

  /** Creates a LoRe Term from a Scala ValDef Tree.
    *
    * @param tree The Scala ValDef Tree.
    * @param termList The list of already-created LoRe terms for this program.
    * @return The LoRe Term.
    */
  private def createLoReTermFromValDef(tree: tpd.ValDef, termList: List[Term])(using
      logLevel: LogLevel,
      ctx: Context
  ): Term = {
    tree match
      case ValDef(name, tpt, rhs) =>
        val loreTypeNode: LoReType = buildLoReTypeNode(tpt.tpe, tpt.sourcePos)
        // Several notes to make here regarding handling the RHS of reactives for future reference:
        // * There's an Apply around the whole RHS whose significance I'm not exactly sure of.
        //   Maybe it's related to a call for Inlining or such, as this plugin runs before that phase
        //   and the expressions being handled here use types that get inlined in REScala.
        // * Because the RHS is wrapped in a call to the respective reactive type, within that unknown Apply layer,
        //   there's one layer of an Apply call to the REScala type wrapping the RHS we want, and the
        //   actual RHS tree we want is inside the second Apply parameter list (i.e. real RHS is 2 Apply layers deep).
        // * As the Derived type uses curly brackets in definitions on top, it additionally wraps its RHS in a Block type.
        // * The Source and Derived parameter lists always have length 1, those of Interactions always have length 2.
        // * Typechecking for whether all of this is correct is already done by the Scala type-checker before this phase,
        //   so we can assume everything we see here is of suitable types instead of doing any further checks.
        // * The above was previously handled here directly, matching on the rhs variable and then handling any
        //   Source/Derived definitions here to shortcut any function calls, but this is now moved and also handled
        //   as part of the same buildLoReRhsTerm function that handles everything else too.
        loreTypeNode match
          case SimpleType(typeName, typeArgs) =>
            if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
              println(s"Detected $typeName definition with name \"$name\"")
            }

            TAbs(                                 // foo: Bar = baz
              name.toString,                      // foo (any valid Scala identifier)
              loreTypeNode,                       // Bar
              buildLoReRhsTerm(rhs, termList, 1), // baz (e.g. 0, 1 + 2, "test", true, 2 > 1, bar reference, etc)
              scalaSourcePos = Some(tree.sourcePos)
            )
          case TupleType(types) =>
            if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
              println(s"Detected Tuple definition with name \"$name\"")
            }

            rhs match
              case Apply(tupleDef @ TypeApply(Select(Ident(tpName), _), _), tupleContents)
                  if tpName.toString.matches("Tuple\\d+") =>
                TAbs(
                  name.toString,
                  loreTypeNode,
                  TTuple(
                    tupleContents.map(tupleElement => {
                      buildLoReRhsTerm(tupleElement, termList, 1)
                    }),
                    scalaSourcePos = Some(tupleDef.sourcePos)
                  ),
                  scalaSourcePos = Some(tree.sourcePos)
                )
              case _ =>
                report.error(s"Detected tuple type with non-tuple RHS:", tree.sourcePos)
                TVar("<error>")
  }

  /** Builds a LoRe Type node based on a Scala type tree
    *
    * @param typeTree  The Scala type tree
    * @param sourcePos A SourcePosition for the type tree
    * @return The LoRe Type node
    */
  private def buildLoReTypeNode(typeTree: ScalaType, sourcePos: SourcePosition)(using
      logLevel: LogLevel,
      ctx: Context
  ): LoReType = {
    typeTree match
      case TypeRef(_, _) => // Non-parameterized types (e.g. Int, String)
        SimpleType(typeTree.asInstanceOf[CachedTypeRef].name.toString, List())
      case AppliedType(outerType: CachedTypeRef, args: List[ScalaType]) => // Parameterized types like List, Map, etc
        // Interactions have various different type names depending on what parts they have
        // For some reason, probably due to the type definitions in UnboundInteraction/Interaction, Interactions with
        // requires show up as type "T" and those with executes as type "E", so those have to be added to this list.
        // This feels fragile, but it appears that is how it will have to be to make things work.
        // Also output any other Interaction types as plain "Interaction", as this type name is not backend-relevant.
        val interactionTypeList: List[String] = List(
          "BoundInteraction",
          "Interaction",
          "InteractionWithActs",
          "InteractionWithExecutes",
          "InteractionWithExecutesAndActs",
          "InteractionWithExecutesAndModifies",
          "InteractionWithModifies",
          "InteractionWithModifiesAndActs",
          "UnboundInteraction",
          "T",
          "E"
        )
        val typeString: String = if interactionTypeList.contains(outerType.name.toString) then "Interaction"
        else if interactionTypeList.exists(t => outerType.prefix.typeConstructor.show.contains(t)) then "Interaction"
        else outerType.name.toString

        if typeString.matches("Tuple\\d+") then {
          // Can get the arity from the type by simply checking the type name
          val tupleArity: Int = typeString.split("Tuple").last.toInt

          if tupleArity > 1 then {
            // Actual tuple: More than 1 element
            val typeList: Option[NonEmptyList[LoReType]] =
              NonEmptyList.fromList(args.map(t => buildLoReTypeNode(t, sourcePos)))

            typeList match
              case None =>
                report.error(s"Detected tuple type without any type elements in AST:\n$typeTree", sourcePos)
                SimpleType("<error>", List())
              case Some(l) => TupleType(l)
          } else {
            // Not actually a tuple: Just 1 element, so return that type instead
            buildLoReTypeNode(args.head, sourcePos)
          }
        } else {
          SimpleType(typeString, args.map(t => buildLoReTypeNode(t, sourcePos)))
        }
      case _ =>
        report.error(s"An error occurred building the LoRe type for the following tree:\n$typeTree", sourcePos)
        SimpleType("<error>", List())
  }

  /** Takes the tree for a Scala RHS value and builds a LoRe term for it.
    *
    * @param tree        The Scala AST Tree node for a RHS expression to convert
    * @param termList The list of already-created LoRe terms for this program.
    * @param indentLevel How many tabs to add before the logs of this call (none by default)
    * @param operandSide Which side to refer to in logs if expressing binary expressions (none by default)
    * @return The corresponding LoRe AST Tree node of the RHS
    */
  private def buildLoReRhsTerm(
      tree: tpd.LazyTree,
      termList: List[Term],
      indentLevel: Integer = 0,
      operandSide: String = ""
  )(using
      logLevel: LogLevel,
      ctx: Context
  ): Term = {
    tree match
      case number @ Literal(Constant(num: Int)) => // Basic int values like 0 or 1
        if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
          logRhsInfo(indentLevel, operandSide, "literal integer value", num.toString)
        }
        TNum(num, scalaSourcePos = Some(number.sourcePos))
      case string @ Literal(Constant(str: String)) => // Basic string values like "foo"
        if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
          logRhsInfo(indentLevel, operandSide, "literal string value", str)
        }
        TString(str, scalaSourcePos = Some(string.sourcePos))
      case boolean @ Literal(Constant(bool: Boolean)) => // Basic boolean values true or false
        if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
          logRhsInfo(indentLevel, operandSide, "literal boolean value", bool.toString)
        }
        if bool then TTrue(scalaSourcePos = Some(boolean.sourcePos))
        else TFalse(scalaSourcePos = Some(boolean.sourcePos))
      case ident @ Ident(referenceName: Name) => // References to variables (any type)
        if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
          logRhsInfo(indentLevel, operandSide, "reference to variable", referenceName.toString)
        }
        // No need to check whether the reference specified here actually exists, because if it didn't
        // then the original Scala code would not have compiled due to invalid reference and this
        // point would not have been reached either way, so just pass on the reference name to a TVar
        TVar(referenceName.toString, scalaSourcePos = Some(ident.sourcePos))
      case fieldUnaryTree @ Select(arg, opOrField) => // Field access and unary operator applications
        opOrField match
          case nme.UNARY_! => // Overall case catching supported unary operators, add other unary operators via |s here
            if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
              logRhsInfo(indentLevel, operandSide, "unary operator application of", opOrField.show)
            }

            opOrField match // Match individual unary operators
              // This specifically has to be nme.UNARY_! and not e.g. nme.NOT
              case nme.UNARY_! => TNeg(
                  buildLoReRhsTerm(arg, termList, indentLevel + 1, operandSide),
                  scalaSourcePos = Some(fieldUnaryTree.sourcePos)
                ) // !operand
              case _ => // Unsupported unary operators
                report.error(
                  s"Unsupported unary operator ${opOrField.show} used:",
                  fieldUnaryTree.sourcePos
                )
                TVar("<error>")
          case field => // Field access, like "operand.value" and so forth (no parameter lists)
            // Unary operators that aren't explicitly supported will also land here and be turned
            // into property/method access AST nodes instead, which makes sense given the Scala base
            // as operators are basically just methods on the data types themselves in the first place.
            if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
              logRhsInfo(indentLevel, operandSide, "field access to field", opOrField.show)
            }

            TFCall(                                                          // foo.bar
              buildLoReRhsTerm(arg, termList, indentLevel + 1, operandSide), // foo (might be a more complex expression)
              field.toString,                                                // bar
              None,                                                          // None (field) vs Empty List (0-ar method)
              scalaSourcePos = Some(fieldUnaryTree.sourcePos)
            )
      case invariantTree @ Apply(Select(Ident(name), method), invExpr)
          if name.toString == "Invariant" && method.toString == "apply" =>
        if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
          logRhsInfo(indentLevel, operandSide, "definition of an Invariant", "")
        }
        val invTerm: Term = buildLoReRhsTerm(invExpr.head, termList, indentLevel, operandSide)
        invTerm match
          case expr: TBoolean =>
            TInvariant(expr, scalaSourcePos = Some(invariantTree.sourcePos))
          case _ =>
            report.error(
              s"Invariant term is not a boolean expression",
              invariantTree.sourcePos
            )
            TVar("<error>")
      case methodBinaryTree @ Apply(Select(leftArg, opOrMethod), params: List[?]) =>
        // Method calls and binary operator applications
        opOrMethod match
          case nme.ADD | nme.SUB | nme.MUL | nme.DIV | nme.And | nme.Or | nme.LT | nme.GT | nme.LE | nme.GE | nme.EQ | nme.NE =>
            // Supported Binary operator applications (as operator applications are methods on types, like left.+(right), etc)
            if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
              logRhsInfo(indentLevel, operandSide, "operator application of operator", opOrMethod.show)
            }

            val rightArg = params.head
            opOrMethod match
              case nme.ADD =>
                TAdd( // left + right
                  buildLoReRhsTerm(leftArg, termList, indentLevel + 1, "left"),
                  buildLoReRhsTerm(rightArg, termList, indentLevel + 1, "right"),
                  scalaSourcePos = Some(methodBinaryTree.sourcePos)
                )
              case nme.SUB =>
                TSub( // left - right
                  buildLoReRhsTerm(leftArg, termList, indentLevel + 1, "left"),
                  buildLoReRhsTerm(rightArg, termList, indentLevel + 1, "right"),
                  scalaSourcePos = Some(methodBinaryTree.sourcePos)
                )
              case nme.MUL =>
                TMul( // left * right
                  buildLoReRhsTerm(leftArg, termList, indentLevel + 1, "left"),
                  buildLoReRhsTerm(rightArg, termList, indentLevel + 1, "right"),
                  scalaSourcePos = Some(methodBinaryTree.sourcePos)
                )
              case nme.DIV =>
                TDiv( // left / right
                  buildLoReRhsTerm(leftArg, termList, indentLevel + 1, "left"),
                  buildLoReRhsTerm(rightArg, termList, indentLevel + 1, "right"),
                  scalaSourcePos = Some(methodBinaryTree.sourcePos)
                )
              case nme.And => TConj( // left && right, Important: nme.AND is & and nme.And is &&
                  buildLoReRhsTerm(leftArg, termList, indentLevel + 1, "left"),
                  buildLoReRhsTerm(rightArg, termList, indentLevel + 1, "right"),
                  scalaSourcePos = Some(methodBinaryTree.sourcePos)
                )
              case nme.Or => TDisj( // left || right, Important: nme.OR is | and nme.Or is ||
                  buildLoReRhsTerm(leftArg, termList, indentLevel + 1, "left"),
                  buildLoReRhsTerm(rightArg, termList, indentLevel + 1, "right"),
                  scalaSourcePos = Some(methodBinaryTree.sourcePos)
                )
              case nme.LT =>
                TLt( // left < right
                  buildLoReRhsTerm(leftArg, termList, indentLevel + 1, "left"),
                  buildLoReRhsTerm(rightArg, termList, indentLevel + 1, "right"),
                  scalaSourcePos = Some(methodBinaryTree.sourcePos)
                )
              case nme.GT =>
                TGt( // left > right
                  buildLoReRhsTerm(leftArg, termList, indentLevel + 1, "left"),
                  buildLoReRhsTerm(rightArg, termList, indentLevel + 1, "right"),
                  scalaSourcePos = Some(methodBinaryTree.sourcePos)
                )
              case nme.LE =>
                TLeq( // left <= right
                  buildLoReRhsTerm(leftArg, termList, indentLevel + 1, "left"),
                  buildLoReRhsTerm(rightArg, termList, indentLevel + 1, "right"),
                  scalaSourcePos = Some(methodBinaryTree.sourcePos)
                )
              case nme.GE =>
                TGeq( // left >= right
                  buildLoReRhsTerm(leftArg, termList, indentLevel + 1, "left"),
                  buildLoReRhsTerm(rightArg, termList, indentLevel + 1, "right"),
                  scalaSourcePos = Some(methodBinaryTree.sourcePos)
                )
              case nme.EQ =>
                TEq( // left == right
                  buildLoReRhsTerm(leftArg, termList, indentLevel + 1, "left"),
                  buildLoReRhsTerm(rightArg, termList, indentLevel + 1, "right"),
                  scalaSourcePos = Some(methodBinaryTree.sourcePos)
                )
              case nme.NE =>
                TIneq( // left != right
                  buildLoReRhsTerm(leftArg, termList, indentLevel + 1, "left"),
                  buildLoReRhsTerm(rightArg, termList, indentLevel + 1, "right"),
                  scalaSourcePos = Some(methodBinaryTree.sourcePos)
                )
              case _ => // Unsupported binary operators
                report.error(
                  s"Unsupported binary operator ${opOrMethod.show} used",
                  methodBinaryTree.sourcePos
                )
                TVar("<error>")
          case methodName => // Method calls outside of explicitly supported binary operators
            if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
              logRhsInfo(indentLevel, operandSide, s"call to a method with ${params.size} params:", methodName.toString)
            }

            TFCall(                                                              // foo.bar(baz, qux, ...)
              buildLoReRhsTerm(leftArg, termList, indentLevel + 1, operandSide), // foo (might be a more complex term)
              methodName.toString,                                               // bar
              Some(params.map(p =>                                               // baz, qux, ... (maybe complex terms)
                buildLoReRhsTerm(p, termList, indentLevel + 1, operandSide)
              )),
              scalaSourcePos = Some(methodBinaryTree.sourcePos)
            )
      case funcCallTree @ Apply(Ident(name: Name), params: List[?]) => // Function calls
        if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
          logRhsInfo(indentLevel, operandSide, s"call to a function with ${params.size} params:", name.toString)
        }

        TFunC(            // foo(bar, baz)
          name.toString,  // foo
          params.map(p => // bar, baz, ... (might each be more complex terms)
            buildLoReRhsTerm(p, termList, indentLevel + 1, operandSide)
          ),
          scalaSourcePos = Some(funcCallTree.sourcePos)
        )
      case instTree @ Apply(
            TypeApply(Select(Ident(typeName: Name), _), _),
            List(Typed(SeqLiteral(params: List[?], _), _))
          ) =>
        // Type instantiations like Lists etc, i.e. specifically "List(...)" and so forth
        if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
          logRhsInfo(indentLevel, operandSide, s"type call to ${typeName.toString} with ${params.length} params", "")
        }

        TFunC(
          typeName.toString,
          params.map(p => buildLoReRhsTerm(p, termList, indentLevel + 1, operandSide)),
          scalaSourcePos = Some(instTree.sourcePos)
        )
      case tupleTree @ Apply(TypeApply(Select(Ident(typeName: Name), _), _), params: List[?])
          if typeName.toString.matches("Tuple\\d+") =>
        if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
          logRhsInfo(indentLevel, operandSide, s"tuple call to $typeName with ${params.length} members", "")
        }

        TTuple(
          params.map(p => buildLoReRhsTerm(p, termList, indentLevel + 1, operandSide)),
          scalaSourcePos = Some(tupleTree.sourcePos)
        )
      case rawInteractionTree @ TypeApply(Select(Ident(tpName), _), _) if tpName.toString == "Interaction" =>
        // Raw Interaction definitions (without method calls) on the RHS, e.g. Interaction[Int, String]
        // This probably breaks if you alias/import Interaction as a different name, not sure how to handle that
        // When not checking for type name, this would match all types you apply as "TypeName[TypeParam, ...]"
        val rhsType: LoReType = buildLoReTypeNode(rawInteractionTree.tpe, rawInteractionTree.sourcePos)
        rhsType match
          case SimpleType(loreTypeName, List(reactiveType, argumentType)) =>
            // reactiveType and argumentType are based on the type tree here, not the types written in the RHS.
            if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
              logRhsInfo(indentLevel, operandSide, s"definition of a $loreTypeName reactive", "")
            }

            TInteraction(
              reactiveType,
              argumentType,
              scalaSourcePos = Some(rawInteractionTree.sourcePos)
            )
          case _ =>
            report.error(
              s"Error building RHS interaction term",
              rawInteractionTree.sourcePos
            )
            TVar("<error>")
      case reactiveTree @ Apply(
            Apply(
              TypeApply(Select(reactive, reactiveMethodName), _),
              reactiveParamTree: List[?]
            ),
            outerReactiveParamTree: List[?],
          ) =>
        // Method calls on Interactions (requires, ensures, modifies, executes)
        val rhsType: LoReType = buildLoReTypeNode(reactiveTree.tpe, reactiveTree.sourcePos)
        rhsType match
          case SimpleType(loreTypeName, typeArgs) =>
            loreTypeName match
              case "Var" =>
                if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
                  logRhsInfo(indentLevel, operandSide, s"definition of a $loreTypeName reactive", "")
                }
                TSource(
                  buildLoReRhsTerm(reactiveParamTree.head, termList, indentLevel + 1, operandSide),
                  scalaSourcePos = Some(reactiveTree.sourcePos)
                )
              case "Signal" =>
                if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
                  logRhsInfo(indentLevel, operandSide, s"definition of a $loreTypeName reactive", "")
                }
                TDerived(
                  buildLoReRhsTerm(reactiveParamTree.head, termList, indentLevel + 1, operandSide),
                  scalaSourcePos = Some(reactiveTree.sourcePos)
                )
              case "Interaction" =>
                if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
                  logRhsInfo(indentLevel, operandSide, s"call to the $reactiveMethodName Interaction method", "")
                }

                // The DSL definitions of Interactions have lots of magic going on, so there's AST variations.
                // Whenever you call "modifies" on an interaction definition, that AST tree will be wrapped in an
                // implicit "Ex" object. Said implicit object is defined in the Interaction file found in the DSL of
                // the LoRe implementation. This wrapping shifts the position of relevant parts of its Scala AST node.
                // Therefore, some of the variable names may not make sense in the Interaction context.

                var interactionTree: tpd.Tree = tpd.EmptyTree
                var methodParamTree: tpd.Tree = tpd.EmptyTree

                reactive match
                  case Ident(name) if name.toString == "Ex" =>
                    // Implicit Ex case
                    interactionTree = reactiveParamTree.head
                    methodParamTree = outerReactiveParamTree.head
                  case _ =>
                    // Any other Interaction method calls are regular
                    interactionTree = reactive
                    methodParamTree = reactiveParamTree.head

                // Build Interaction using the correctly set trees
                buildLoReInteractionTerm(
                  reactiveTree,
                  interactionTree,
                  methodParamTree,
                  reactiveMethodName,
                  termList,
                  indentLevel,
                  operandSide
                )
              case _ =>
                report.error(s"Error building RHS term", reactiveTree.sourcePos)
                TVar("<error>")
          case _ =>
            report.error(s"Error building RHS term", reactiveTree.sourcePos)
            TVar("<error>")
      case arrowTree @ Block(List(DefDef(name, List(lhs), _, rhs)), _) if name.toString == "$anonfun" =>
        // Arrow funcs: When all parameters are defined, the function is in the first parameter
        // Duplicated code from below, but seems that is how it has to be. Can't bind in alternatives, after all.
        buildLoReArrowTerm(arrowTree, lhs, rhs, termList, indentLevel, operandSide)
      case arrowTree2 @ Block(_, Block(List(DefDef(name, List(lhs), _, rhs)), _)) if name.toString == "$anonfun" =>
        // Arrow funcs 2: When a parameter is wildcarded (i.e. via _), the function is in the second parameter instead
        // Duplicated code from above, but seems that is how it has to be. Can't bind in alternatives, after all.
        buildLoReArrowTerm(arrowTree2, lhs, rhs, termList, indentLevel, operandSide)
      case blockTree @ Block(blockBody, blockReturn) => // Blocks of statements (e.g. in arrow functions)
        if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
          logRhsInfo(indentLevel, operandSide, s"block of statements", "")
        }

        if blockBody.nonEmpty then {
          // Multiple statements in this block: Build a LoRe sequence of statements
          val blockList: NonEmptyList[Term] = NonEmptyList.fromList(
            blockBody.map(b => buildLoReRhsTerm(b, termList, indentLevel, operandSide))
            :+ buildLoReRhsTerm(blockReturn, termList, indentLevel, operandSide)
          ).get
          TSeq(blockList, scalaSourcePos = Some(blockTree.sourcePos))
        } else {
          // Only a single statement in this block: Just return the one statement by itself
          buildLoReRhsTerm(blockReturn, termList, indentLevel, operandSide)
        }
      case ifTree @ If(cond, _then, _else) => // If conditions (e.g. "if x then y else z")
        if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
          logRhsInfo(indentLevel, operandSide, s"conditional expression", "")
        }

        val condTerm: Term = buildLoReRhsTerm(cond, termList, indentLevel, operandSide)  // if x
        val thenTerm: Term = buildLoReRhsTerm(_then, termList, indentLevel, operandSide) // then y
        val elseTerm: Option[Term] = _else match // else z
          case Literal(Constant(_: Unit)) => None // No else exists
          case _ => Some(buildLoReRhsTerm(_else, termList, indentLevel, operandSide)) // Else exists

        TIf(condTerm, thenTerm, elseTerm, scalaSourcePos = Some(ifTree.sourcePos))
      case t: Tree[?] => // Unsupported RHS forms
        report.error(s"Unsupported RHS form used", t.sourcePos)
        TVar("<error>")
      case _ => // This case shouldn't be hit, but compiler warns about potentially inexhaustive matches.
        report.error(s"Unsupported RHS form used:\n$tree")
        TVar("<error>")
  }

  // See the reactiveTree case in buildLoReRhsTerm, code extracted to avoid duplication
  private def buildLoReInteractionTerm(
      tree: tpd.Tree,
      interactionTree: tpd.Tree,
      methodParamTree: tpd.Tree,
      methodName: Name,
      termList: List[Term],
      indentLevel: Integer,
      operandSide: String,
  )(using logLevel: LogLevel, ctx: Context): Term = {
    val interactionTerm: Term = buildLoReRhsTerm(interactionTree, termList, indentLevel + 1, operandSide)
    val methodParamTerm: Term = buildLoReRhsTerm(methodParamTree, termList, indentLevel + 1, operandSide)

    interactionTerm match
      case interaction: TInteraction =>
        // Call on an Interaction literal (not a reference to an already-defined interaction),
        // so build upon this Interaction literal and attach the new method call to the literal.
        addMethodToInteractionTerm(tree, interaction, methodParamTerm, methodName.toString)
      case interactionReference @ TVar(name, _, _) =>
        // The interaction term is a reference to an already-defined Interaction, so find the
        // definition of this Interaction in the list of terms, copy that definition, and then
        // modify it to include the additionally called method on it like above.
        val interactionDef: Option[Term] = termList.find {
          case TAbs(`name`, _, body: TInteraction, _, _) => true
          case _                                         => false
        }

        interactionDef match
          case Some(TAbs(_, _, interaction: TInteraction, _, _)) =>
            addMethodToInteractionTerm(tree, interaction, methodParamTerm, methodName.toString)
          case _ => // Matching on both "None" and "Some"s of any different shape than above
            // No definition found in the list of already-processed terms, which means it must be a
            // forward reference. If it didn't exist at all, the Scala compiler would have errored out
            // in a previous phase already, before this point would ever have been reached.
            report.error(
              s"Could not find Interaction definition for this reference. Forward references are not allowed.",
              tree.sourcePos
            )
            TVar("<error>")
      // Other cases are errors
      case _ =>
        report.error(s"Error building RHS term for Interaction", tree.sourcePos)
        TVar("<error>")
  }

  private def addMethodToInteractionTerm(
      tree: tpd.Tree,
      interaction: TInteraction,
      methodParamTerm: Term,
      methodName: String
  )(using @unused logLevel: LogLevel, ctx: Context): TInteraction = {
    // To add a method to an interaction term, copy the existing term and add the method term to the respective field.
    // Also update the source position of the interaction term with that of the supplied tree - this is important if
    // the interaction term being added to is a reference (i.e. an interaction is being defined that is an existing
    // interaction, plus an extra method call), otherwise any verification errors relating to it would be reported at
    // the referenced interaction in Scala/LoRe, not at the definition of the interaction which has the reference.

    methodName match
      // modifies is handled specifically in above case due to different structure
      case "modifies" =>
        methodParamTerm match
          case TVar(modVar, _, _) => interaction.copy(
              modifies = interaction.modifies.prepended(modVar),
              scalaSourcePos = Some(tree.sourcePos)
            )
          case _ =>
            report.error(
              s"Error building RHS term for Interaction modifies call",
              tree.sourcePos
            )
            interaction
      case "requires" =>
        interaction.copy(
          requires = interaction.requires.prepended(methodParamTerm),
          scalaSourcePos = Some(tree.sourcePos)
        )
      case "ensures" =>
        interaction.copy(
          ensures = interaction.ensures.prepended(methodParamTerm),
          scalaSourcePos = Some(tree.sourcePos)
        )
      case "executes" =>
        // executes can only have one value due to being an Option, so simply replace the value
        interaction.executes match
          case None => interaction.copy(executes = Some(methodParamTerm), scalaSourcePos = Some(tree.sourcePos))
          case Some(seq: TSeq) =>
            // Sequence already exists, so append to it
            val newSeq: TSeq = seq.copy(body = seq.body.append(methodParamTerm))
            interaction.copy(executes = Some(newSeq), scalaSourcePos = Some(tree.sourcePos))
          case Some(term: Term) =>
            // Executes is currently a single term, so form sequence
            val seqBody: NonEmptyList[Term] = NonEmptyList.of(term, methodParamTerm)
            val seq: TSeq                   = TSeq(seqBody, scalaSourcePos = Some(tree.sourcePos))
            interaction.copy(executes = Some(seq), scalaSourcePos = Some(tree.sourcePos))
  }

  // See the two arrowTree cases in buildLoReRhsTerm, code extracted to avoid duplication
  private def buildLoReArrowTerm(
      tree: tpd.Tree,
      lhs: List[tpd.Tree],
      rhs: tpd.LazyTree,
      termList: List[Term],
      indentLevel: Integer,
      operandSide: String
  )(using logLevel: LogLevel, ctx: Context): TArrow = {
    if logLevel.isLevelOrHigher(LogLevel.Verbose) then {
      logRhsInfo(indentLevel, operandSide, s"arrow function with ${lhs.length} arguments", "")
    }

    TArrow(            // (foo: Int) => foo + 1
      TTuple(lhs.map { // (foo: Int)
        case argTree @ ValDef(paramName, paramType, tpd.EmptyTree) =>
          TArgT(
            paramName.toString,
            buildLoReTypeNode(paramType.tpe, paramType.sourcePos),
            scalaSourcePos = Some(argTree.sourcePos)
          )
        case _ =>
          report.error(
            s"Error building LHS term for arrow function",
            tree.sourcePos
          )
          TVar("<error>")
      }),
      buildLoReRhsTerm(rhs, termList, indentLevel + 1, operandSide), // foo + 1
      scalaSourcePos = Some(tree.sourcePos)
    )
  }
}
