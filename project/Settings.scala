/* This file is shared between multiple projects
 * and may contain unused dependencies */

import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.jsEnv
import sbt.*
import sbt.Keys.*

object Settings {

  // also consider updating the -source param below
  val scala3VersionString = sys.env.get("SCALA_VERSION").filter(!_.isBlank).getOrElse("3.7.3")

  // needs either 3.7 or 3.5 minor version in 3.6, otherwise there is a unfixable warning about changed implicit order
  // see https://github.com/scala/scala3/issues/22153
  val scala3VersionMinor = scala3VersionString.reverse.dropWhile(c => c != '.').drop(1).reverse

  // see https://docs.scala-lang.org/overviews/compiler-options/
  // and https://docs.scala-lang.org/scala3/guides/migration/options-new.html
  // and https://www.scala-lang.org/api/current/scala/language$.html
  // and run: cs launch scala3-compiler -- -help

  val scala3defaults = Def.settings(
    scalaVersion := scala3VersionString,
    fullFeatureDeprecationWarnings,
    scalaSourceLevel(scala3VersionMinor),
    warningsAreErrors(Compile / compile, Test / compile),
    valueDiscard(Compile / compile),
    typeParameterShadow(Compile / compile),
    privateShadow(Compile / compile),
    recurseWithDefault(Compile / compile, Test / compile),
    semanticdbEnabled := true,
    unusedWarnings(Compile / compile, Test / compile)
  )

  val scala3defaultsExtra = Def.settings(
    scala3defaults,
    javaOutputVersion(17),
    explicitNulls(Compile / compile),
    safeInit(Compile / compile),
    unstableInlineAccessors(Compile / compile),
  )

  // Spell out feature and deprecation warnings instead of summarizing them into a single warning
  // always turn this on to make the compiler less ominous
  def fullFeatureDeprecationWarnings = scalacOptions ++= List("-feature", "-deprecation")

  // set a specific source level for warnings/rewrites/features
  // generally recommended to get consistent behaviour
  def scalaSourceLevel(level: String) = scalacOptions ++= List("-source", level)

  // defines the output classfile version, and disables use of newer methods from the JDK classpath
  def javaOutputVersion(n: Int, conf: TaskKey[?]*) = Def.settings(
    taskSpecificScalacOption("-java-output-version", conf*),
    taskSpecificScalacOption(n.toString, conf*)
  )

  // treat warnings as errors
  // generally, adressing warnings as they come up is much less work than fixing problems later
  // do consider disabling for migrations and large refactorings to allow those changes to happen in smaller steps
  def warningsAreErrors(conf: TaskKey[?]*) = taskSpecificScalacOption("-Werror", conf*)

  // seems generally unobtrusive (just add some explicit ()) and otherwise helpful
  def valueDiscard(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wvalue-discard", conf*)

  // can be annoying with methods that have optional results, can also help with methods that have non optional results …
  def nonunitStatement(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wnonunit-statement", conf*)

  // reports methods that have public forwarders (in the binaries) because they are accessed by an inline function
  def unstableInlineAccessors(conf: TaskKey[?]*) = taskSpecificScalacOption("-WunstableInlineAccessors", conf*)

  // type parameter shadowing often is accidental, and especially for short type names keeping them separate seems good
  def typeParameterShadow(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wshadow:type-parameter-shadow", conf*)

  // shadowing fields causes names inside and outside of the class to resolve to different things, and is quite weird.
  // however, this has some kinda false positives when subclasses pass parameters to superclasses.
  def privateShadow(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wshadow:private-shadow", conf*)

  // checks that objects are fully initialized before they are accessed
  // is kinda likely to cause strange compiler crashes, disable if something is strange
  // (was -Ysafe-init for scala 3.4 and below)
  def safeInit(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wsafe-init", conf*)

  // this prevents recursive calls that use any of the default parameters.
  // the hope is, that this allows to have some accumulater default to empty, but then not forget to update it during recursion
  def recurseWithDefault(conf: TaskKey[?]*) = taskSpecificScalacOption("-Wrecurse-with-default", conf*)

  // makes Null no longer be a sub type of all subtypes of AnyRef
  // since Scala 3.5 uses special return types for Java methods, see https://github.com/scala/scala3/pull/17369
  // disable special handling with -Yno-flexible-types
  def explicitNulls(conf: TaskKey[?]*) = taskSpecificScalacOption("-Yexplicit-nulls", conf*)

  // Enforce then and do syntax, combine with rewrite to automatically rewrite
  def newSyntax = scalacOptions += "-new-syntax"

  // combine with -new-syntax, -indent, or -source some-migration to rewrite changed behavior
  def rewrite = scalacOptions += "-rewrite"

  // allow definition and application of implicit conversions
  def implicitConversions(conf: TaskKey[?]*) = taskSpecificScalacOption("-language:implicitConversions", conf*)

  // require an instance of Eql[A, B] to allow == checks. This is rather invasive, but would be a great idea if more widely supported …
  def strictEquality(conf: TaskKey[?]*) = taskSpecificScalacOption("-language:strictEquality", conf*)

  // this unused warnings definition is meant to be enabled only sometimes when looking for unused elements.
  // It does not play well with -Werror and makes developing quite annoying.
  def unusedWarnings(conf: TaskKey[?]*) = {
    val c2 = if (conf.isEmpty) List(Compile / compile, Test / compile) else conf
    c2.map { c =>
      c / scalacOptions ++= List(
        // Warn for unused @nowarn annotations
        "-Wunused:nowarn",
        // Warn if an import selector is not referenced.
        "-Wunused:imports",
        // Same as -Wunused:import, only for imports of explicit named members. NOTE : This overrides -Wunused:imports and NOT set by -Wunused:all,
        "-Wunused:strict-no-implicit-warn",
        // Warn if a private member is unused,
        "-Wunused:privates",
        // Warn if a local definition is unused,
        // "-Wunused:locals",
        // Warn if an explicit parameter is unused,
        // "-Wunused:explicits",
        // Warn if an implicit parameter is unused,
        "-Wunused:implicits",
        // (UNSAFE) Warn if a variable bound in a pattern is unused. This warning can generate false positive, as warning cannot be suppressed yet.
        // "-Wunused:unsafe-warn-patvars",
        // also make unused warnings not warnings but just infos
        "-Wconf:id=E198:info,msg=annotation does not suppress any warnings but matches a diagnostic:info",
      )
    }
  }

  def taskSpecificScalacOption(setting: String, conf: TaskKey[?]*) = {
    val c2 = if (conf.isEmpty) List(Compile / compile, Test / compile) else conf
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
  val jsEnvDom = jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

}
