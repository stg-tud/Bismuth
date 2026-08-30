/* This file is shared between multiple projects
 * and may contain unused dependencies */

import sbt.*
import sbt.Keys.*

object Settings {
  // see https://docs.scala-lang.org/overviews/compiler-options/
  // and https://docs.scala-lang.org/scala3/guides/migration/options-new.html
  // and https://www.scala-lang.org/api/current/scala/language$.html
  // and run: cs launch scala3-compiler -- -help

  val scala3defaults = Def.settings(
    fullFeatureDeprecationUncheckedWarnings,
    semanticdbEnabled := true,
    warningsAreErrors(),
    valueDiscard(),
    typeParameterShadow(),
    privateShadow(),
    recurseWithDefault(),
    unusedWarnings(),
    enumCommentDiscard(),
    implausiblePatterns(),
    newSyntax(),
  )

  val scala3defaultsExtra = Def.settings(
    scala3defaults,
    explicitNulls(),
    safeInit(Compile / compile),
    unstableInlineAccessors(),
  )


  // the inline defs are to workaround a sbt2 bug (“behaviour”), where seemingly all settings created here are treated as if they were the same, so setting javaOutputVersion to 17 in one project and 21 in another, would cause both to be 21 or 17.

  // set a specific source level for warnings/rewrites/features
  // useful mostly during migrations, otherwise should default to the current scala version
  inline def scalaSourceLevel(inline level: String) = scalacOptions ++= List("-source", level)

  // defines the output classfile version, and disables use of newer methods from the JDK classpath
  inline def javaOutputVersion(inline n: Int, conf: TaskKey[?]*) =
    taskSpecificScalacOption(s"-java-output-version:$n", conf*)

  // Spell out feature and deprecation warnings instead of summarizing them into a single warning
  // always turn this on to make the compiler less ominous
  def fullFeatureDeprecationUncheckedWarnings = scalacOptions ++= List("-feature", "-deprecation", "-unchecked")

  // makes Null no longer be a sub type of all subtypes of AnyRef
  // since Scala 3.5 uses special return types for Java methods, see https://github.com/scala/scala3/pull/17369
  // disable special handling with -Yno-flexible-types
  def explicitNulls(conf: TaskKey[?]*) = taskSpecificScalacOption("-Yexplicit-nulls", conf*)

  // Enforce then and do syntax, combine with rewrite to automatically rewrite
  def newSyntax(conf: TaskKey[?]*) = taskSpecificScalacOption("-new-syntax", conf*)

  // combine with -new-syntax, -indent, or -source some-migration to rewrite changed behavior
  def rewrite(conf: TaskKey[?]*) = taskSpecificScalacOption("-rewrite", conf*)

  // require an instance of Eql[A, B] to allow == checks. This is rather invasive, but would be a great idea if more widely supported …
  def strictEquality(conf: TaskKey[?]*) = taskSpecificScalacOption("-language:strictEquality", conf*)

  // --- warning options, ordered as shown by `cs launch scala3-compiler -- -W` ----

  // treat warnings as errors
  // generally, adressing warnings as they come up is much less work than fixing problems later
  // do consider disabling for migrations and large refactorings to allow those changes to happen in smaller steps
  def warningsAreErrors(conf: TaskKey[?]*) = taskSpecificScalacOption("-Werror", conf*)

  // Warn when a comment ambiguously assigned to multiple enum cases is discarded.
  def enumCommentDiscard(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wenum-comment-discard", conf*)

  // Warn if comparison with a pattern value looks like it might always fail.
  def implausiblePatterns(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wimplausible-patterns", conf*)

  // Warn if a type argument was inferred as union type; often the union is accidental.
  def inferUnion(conf: TaskKey[?]*) = taskSpecificScalacOption("-Winfer-union", conf*)

  // can be annoying with methods that have optional results, can also help with methods that have non optional results …
  def nonunitStatement(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wnonunit-statement", conf*)

  // this prevents recursive calls that use any of the default parameters.
  // the hope is, that this allows to have some accumulater default to empty, but then not forget to update it during recursion
  def recurseWithDefault(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wrecurse-with-default", conf*)

  // checks that objects are fully initialized before they are accessed
  // is kinda likely to cause strange compiler crashes, disable if something is strange
  // (was -Ysafe-init for scala 3.4 and below)
  def safeInit(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wsafe-init", conf*)

  // shadowing fields causes names inside and outside of the class to resolve to different things, and is quite weird.
  // however, this has some kinda false positives when subclasses pass parameters to superclasses.
  def privateShadow(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wshadow:private-shadow", conf*)

  // type parameter shadowing often is accidental, and especially for short type names keeping them separate seems good
  def typeParameterShadow(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wshadow:type-parameter-shadow", conf*)

  // Warn when a standard interpolator is used to call toString on a reference type.
  def toStringInterpolated(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wtostring-interpolated", conf*)

  // reports methods that have public forwarders (in the binaries) because they are accessed by an inline function
  def unstableInlineAccessors(conf: TaskKey[?]*) = taskSpecificScalacOption("-WunstableInlineAccessors", conf*)

  // seems generally unobtrusive (just add some explicit ()) and otherwise helpful
  def valueDiscard(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wvalue-discard", conf*)

  // Warn if the function arrow => was used where a context literal ?=> would work.
  def wrongArrow(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wwrong-arrow", conf*)

  // this unused warnings definition is meant to be enabled only sometimes when looking for unused elements.
  // It does not play well with -Werror and makes developing quite annoying.
  def unusedWarnings(conf: TaskKey[?]*) = {
    val c2 = if conf.isEmpty then List(Compile / compile, Test / compile) else conf
    c2.map { c =>
      c / scalacOptions ++= List(
        // Warn for unused @nowarn annotations
        "-Wunused:nowarn",
        // Warn if an import selector is not referenced.
        "-Wunused:imports",
        // Warn if a private member is unused,
        "-Wunused:privates",
        // Warn if a local definition is unused,
        "-Wunused:locals",
        // Warn if an implicit parameter is unused,
        "-Wunused:implicits",
        // also make unused warnings not warnings but just infos
        "-Wconf:id=E198:info",
      )
    }
  }

  // unused warnings that prevent helpful "named but unused" parameters
  def extraUnusedWarnings(conf: TaskKey[?]*) = {
    val c2 = if conf.isEmpty then List(Compile / compile, Test / compile) else conf
    c2.map { c =>
      c / scalacOptions ++= List(
        // Warn if a variable bound in a pattern is unused,
        "-Wunused:unsafe-warn-patvars",
        // Warn if an explicit parameter is unused,
        "-Wunused:explicits"
      )
    }
  }

  inline def taskSpecificScalacOption(inline setting: String, conf: TaskKey[?]*) = {
    val c2 = if conf.isEmpty then List(Compile / compile, Test / compile) else conf
    c2.map { c => c / scalacOptions += setting }
  }

  // this is a tool to analyse memory consumption/layout
  val jolSettings = Seq(
    libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.17",
    javaOptions += "-Djdk.attach.allowAttachSelf",
    fork := true,
  )

  // see https://www.scala-js.org/doc/project/js-environments.html
  // TLDR: enables the dom API when running on nodejs for the tests
  // val jsEnvDom = jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

}
