import org.scalablytyped.converter.plugin.ScalablyTypedConverterExternalNpmPlugin.autoImport.externalNpm
import org.scalajs.linker.interface.ModuleSplitStyle
import sbt.Keys.baseDirectory
import Tests.*

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "3.5.1"

lazy val root = project
  .in(file("."))
  .aggregate(textrdt.jvm, textrdt.js)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val textrdt = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(
    name := "text-rdt",
    version := "0.1.0-SNAPSHOT",
    scalacOptions ++= Seq(
      "-no-indent",
      //"-Wall",
      "-deprecation",
      "-Yexplicit-nulls",
    ),
    scalacOptions += {
      val baseUrl: String =
        (LocalRootProject / baseDirectory).value.toURI.toString
      s"-scalajs-mapSourceURI:$baseUrl->/"
    },
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.18.1" % Test,
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.2" % Test,
    libraryDependencies += "org.scalameta" %%% "munit-scalacheck" % "1.0.0" % Test,
    libraryDependencies += "com.lihaoyi" %%% "ujson" % "4.0.1",
    libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.9.0",
    libraryDependencies += "com.lihaoyi" %%% "upickle" % "4.0.1",
    assembly / assemblyMergeStrategy := {
      case "module-info.class" => MergeStrategy.discard
      case PathList("META-INF", "versions", xs @ _, "module-info.class") =>
        MergeStrategy.discard
      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    },
    assembly / mainClass := Some("text_rdt.forMemoryProfiling"),
    Test / testOptions += Tests.Argument("-F")
  )
  .jsConfigure(_.enablePlugins(ScalablyTypedConverterExternalNpmPlugin))
  .jvmConfigure(_.enablePlugins(JmhPlugin))
  .jsConfigure(_.enablePlugins(JmhPlugin))
  .jvmSettings(
    libraryDependencies += "com.microsoft.playwright" % "playwright" % "1.47.0"
  )
  .jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0")
      .cross(CrossVersion.for3Use2_13),
    scalaJSUseMainModuleInitializer := true,
    scalaJSMainModuleInitializer := Some(
      org.scalajs.linker.interface.ModuleInitializer
        .mainMethod("text_rdt.JSMain", "main")
    ),
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("text_rdt"))
        )
    },
    externalNpm := baseDirectory.value.getParentFile
  )
