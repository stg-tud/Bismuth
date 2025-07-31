# LoRe Compiler Plugin
> A Scala compiler plugin for verifying Scala-embedded LoRe code through compilation to Dafny.

## Structure

The functionality of this plugin is divided into three parts:

- The compilation frontend (located in the [LoReGen](src/main/scala/loreCompilerPlugin/codegen/LoReGen.scala) file)
- The compilation backend (located in the [DafnyGen](src/main/scala/loreCompilerPlugin/codegen/DafnyGen.scala) file)
- The actual plugin (located in the [LoRePlugin](src/main/scala/loreCompilerPlugin/LoRePlugin.scala) file)

The frontend is responsible for building the LoRe AST nodes from the Scala AST, and the backend is responsible for generating Dafny code from the LoRe AST nodes. The plugin itself is responsible for calling the frontend to generate said LoRe AST nodes, passing those to the backend for Dafny generation as well as instantiating and communicating with the Dafny LSP implementation to receive verification results.

The frontend additionally makes use of a custom [LoReProgram](src/main/scala/loreCompilerPlugin/annotation/LoReProgram.scala) annotation, and the backend makes use of a [client](src/main/scala/loreCompilerPlugin/lsp/DafnyLSPClient.scala) and as well as [datastructures](src/main/scala/loreCompilerPlugin/lsp/LSPDataTypes.scala) for interacting with the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) implementation of Dafny.

## Usage

Usage of this plugin requires installation of [Dafny](https://dafny.org/), which includes adding the path of its executable to your PATH. Additionally, you must then navigate to the `z3/bin` folder of your Dafny installation and copy one of the given executables (e.g. `z3-4.8.5`) to the root of your Dafny installation folder (i.e. the folder Dafny's own executable is in), renaming the copied executable to `z3`. The reason for this is that Dafny's LSP implementation will otherwise fail to find the Z3 executable on boot. This project was developed under usage of Z3 version 4.8.5, other versions are untested.

After installation, you can try compiling the given [example files](examples/src/main/scala/loreCompilerPlugin) via `loreCompilerPluginExamples/clean;loreCompilerPluginExamples/compile` in an sbt shell. On first run, this will compile both LoRe itself and the compiler plugin, and then compile the given example files under usage of the compiler plugin. By default, logging will be restricted to essential messages. This behavior may be controlled to log more (or less) messages by setting the logging level in the plugin file described in the above section on structure.

## Features

### Compilation frontend

The compilation frontend supports generation of the following LoRe AST nodes from Scala-embedded LoRe code:

- TArgT (from function and method arguments, e.g. the `x: Int, y: String` part in `def foo(x: Int, y: String): ...`)
- TVar (references to existing definitions)
  - Even though Scala supports forward references (marking them as warnings, but supporting them nonetheless), the backend does not support these and will error upon detection
- TAbs (definitions in the Scala form `val foo: bar = baz`)
- TTuple (tuple values such as `(foo, bar)` in Scala, with arbitrary tuple length)
- TIf (if statements, such as `if foo then bar else baz` in Scala, with or without `else` case, but only as statements, not expressions)
- TSeq (blocks of statements in Scala)
- TArrow (arrow functions in Scala)
- TSource (LoRe Source reactives, realized through the REScala `Var` type)
- TDerived (LoRe Derived reactives, realized through the REScala `Signal` type)
- TInteraction (LoRe Interactions defined as part of the LoRe DSL for Scala, with `modifies`, `requires`, `ensures` and `executes` calls)
  - Only Interactions with one reactive and one parameter (i.e. `Interaction[Foo, Bar]`) are currently supported
  - While the frontend does not place restrictions here, pre- and postconditions (i.e. `requires` and `ensures` calls respectively) may only contain a single expression, else backend generation will error
- TInvariant (LoRe Invariants defined as part of the LoRe DSL for Scala)
- TNum (integer literals), TString (string literals), TTrue and TFalse (boolean base literals)
  - TEq (equality), TIneq (inequality) on integers, strings, booleans
  - TDiv (division), TMul (multiplication), TAdd (addition), TSub (subtraction), TLt (less-than), TGt (greater-than), TLeq (less-equals), TGeq (greater-equals) on integers
  - TNeg (negation), TDisj (OR), TConj (AND) on booleans
- TFCall (property and method access on objects, i.e. `foo.bar` and `foo.bar(...)`)
- TFunC (function calls, i.e. `foo(...)`)
 - Scala type applications (such as literals of types like `List` or `Map`) are also realized using this AST node type

Additionally, it supports both the `SimpleType` and `TupleType` AST nodes for type annotations of definitions.

The compilation frontend currently does not support:

- TViperImport (imports of other classes or the like)
  - Despite the name, this AST node type is not Viper-specific, but a general node for imports
- TTypeAl (type aliasing)
- TAssert (assertions)
- TAssume (assumptions)
- TImpl (logical implications)
- TBImpl (logical equivalencies)
- TInSet (in-set checks for elements on sets)
- TForall (universal quantification)
- TExists (existential quantification)
- TParens (wrapping of expressions in parentheses)
  - This AST type is not necessary when using the LoRe Scala DSL, as nesting is represented in the Scala AST already
- TFCurly
  - The necessity of this AST type is uncertain when using the LoRe Scala DSL, as all features could be implemented without it

### Compilation backend

The compilation backend supports Dafny generation of the following LoRe AST nodes:

- TArgT (analog to their Scala/LoRe variants, of the form `foo: bar` where `foo` is a name and `bar` a type)
- TVar (references to existing definitions)
  - Source and Derived definitions may only be referenced through the `value` and `now` fields
  - References to the `value` property of a Source definition are transformed into plain references to their Dafny field
  - References to the `value` property of a Derived definition are transformed into calls to their model functions
  - References to Invariant definitions are invalid
  - Forward references are invalid
- TAbs (into Dafny `const` definitions for all values apart from Source, Derived, Interaction and Invariant)
  - All definitions aside from Source, Derived, Interaction and Invariant are modelled as immutable `const` fields on the main class
- TTuple (tuple values such as `(foo, bar)`)
- TIf (if statements, such as `if foo then bar else baz` with or without `else` case, but only as statements, not expressions)
- TSeq (blocks of statements)
- TArrow (arrow functions)
- TAssert (assertions)
- TAssume (assumptions)
- TSource (LoRe Source reactives, modelled as `var` fields on a main class)
  - Definitions of Sources are modelled as mutable `var` fields on the main class
- TDerived (LoRe Derived reactives, modelled as Dafny functions)
  - Definitions of Deriveds are modelled as functions whose return type is the parameter type of the Derived
  - All references used in the definition of the Derived are turned into parameters of the model function
- TInteraction (LoRe Interactions, modelled as Dafny methods)
  - Definitions of Interactions are modelled as models without a return type, as these modify the state given as parameter
  - The main class instance containing all fields, as well as the interaction parameter are turned into parameters of the model method
  - Any Invariants relevant for this Interaction are automatically included as a pre- and postcondition for it
  - Each element of the pre- and postconditions list (i.e. each `requires` and `ensures` entry respectively) may only contain a single expression
- TInvariant (LoRe Invariants, modelled as Dafny functions)
  - Definitions of Invariants are modelled as functions whose return type is a boolean
  - All references used in the definition of the Interaction are turned into parameters of the model function
  - This Invariant is automatically included as a pre- and postcondition for Interactions to which it is relevant
- TParens (wrapping of expressions in parentheses)
  - The relevancy of this AST node is uncertain, and is not supported in the frontend, but it is implemented for the backend
- TNum (integer literals), TString (string literals), TTrue and TFalse (boolean base literals)
	- TEq (equality), TIneq (inequality) on integers, strings, booleans
	- TDiv (division), TMul (multiplication), TAdd (addition), TSub (subtraction), TLt (less-than), TGt (greater-than), TLeq (less-equals), TGeq (greater-equals) on integers
	- TNeg (negation), TDisj (OR), TConj (AND), TImpl (logical implications), TBImpl (logical equivalencies), TInSet (in-set checks for elements on sets) on booleans
- TFCall (property and method access on objects, i.e. `foo.bar` and `foo.bar(...)`)
  - Currently, most such calls will cause Dafny compilation errors, as there is no translation of field or method calls into equivalent Dafny variants (if existing)
- TFunC (function calls, i.e. `foo(...)`)
  - Currently, most such calls will cause Dafny compilation errors, as there is no translation of function calls into equivalent Dafny variants (if existing)
- Scala type applications (such as literals of types like `List` or `Map`) are also realized using this AST node type

Additionally, it supports generations of type annotations from both the `SimpleType` and `TupleType` AST nodes.

The compilation backend currently does not support:

- TViperImport (imports of other classes or the like)
  - Despite the name, this AST node type is not Viper-specific, but a general node for imports
- TTypeAl (type aliasing)
- TForall (universal quantification)
- TExists (existential quantification)
- TFCurly
	- The necessity of this AST type is uncertain as Dafny does not require such differentiation between paren types anywhere

Additionally, Source, Derived, Interaction and Invariant literals placed within another expression are not supported, e.g. you may not include any expression such as `foo(Source(1))` or `Source(Derived { 1 })` etc.

Names may also only be used once across a LoReProgram (i.e. a Scala `object` with said annotation), regardless of scoping. This is because of a restriction in the current backend implementation and could be fixed by improving this section of the backend in the future. For example, if you define a `val foo` on the top-level of a LoReProgram, you may not define either a `val foo` within a block contained by an expression of this LoReProgram (e.g. the `then` block of an `if` statement) or as the name of a parameter in e.g. an arrow function (e.g. `(foo) => (...)`), even if that would normally be permissible under a working implementation of scoping. Duplicated top-level definitions are, of course, unsupported as normal.
### Actual plugin

The plugin itself implements the following features:
- Detecting definitions of Scala objects tagged with the "LoReProgram" annotations
- Initiating the frontend on the contents of these definitions and storing its output
- Initiating the backend on the results of the frontend for generation of respective Dafny code and storing its output
- Initiating a client for communication with the Dafny LSP implementation
- Sending the results of the backend to the Dafny LSP implementation for code verification
- Reading out the verification results of the backend code sent to the LSP implementation and reporting any errors to the user via the Scala compiler

## Examples

- The `sourceExamples` file contains example definitions using various syntax with Sources across integers, strings and booleans
- The `derivedExamples` file contains example definitions for function/method/property calls and Derived instantiations
- The `interactionExamples` file contains example definitions for types (List, Map), arrow functions, and Interactions with various method calls on them
- The `test` file contains the test program outlined in the thesis accompanying this compiler plugin.
