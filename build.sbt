import Settings.scala3defaults

import java.net.URI
import scala.scalanative.build.{LTO, Mode}

lazy val bismuth = project.in(file(".")).settings(scala3defaults).aggregate(
  aead.js,
  aead.jvm,
  channels.js,
  channels.jvm,
  dtn.js,
  dtn.jvm,
  deltalens,
  exampleLenses,
  examplesMiscJVM,
  loCal,
  lore.js,
  lore.jvm,
  loreCompilerPlugin,
  microbenchmarks,
  proBench,
  rdts.js,
  rdts.jvm,
  rdts.native,
  reactives.js,
  reactives.jvm,
  reactives.native,
  replication.js,
  replication.jvm,
  replication.native,
  todolist
)

// aggregate projects allow compiling all variants (js, jvm, native) at the same time

lazy val publishedProjects =
  project.in(file("target/PhonyBuilds/publishedProjects")).settings(scala3defaults, publish / skip := true)
    .aggregate(
      rdts.js,
      rdts.jvm,
      rdts.native,
      reactives.js,
      reactives.jvm,
      reactives.native,
      channels.jvm,
      channels.js,
      channels.native,
      replication.jvm,
      replication.js,
      replication.native
    )
    // set publishing settings to have aggregate commands of bundle uploading work,
    // but do not publish this project itselfs
    .settings(SettingsLocal.publishSonatype, publish / skip := true)

// projects in alphabetical order

lazy val aead = crossProject(JSPlatform, JVMPlatform).in(file("Modules/Aead"))
  .jsConfigure(_.enablePlugins(ScalaJSBundlerPlugin))
  .settings(
    scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.munit,
    Dependencies.munitCheck,
  )
  .jvmSettings(
    Dependencies.tink
  )
  .jsSettings(
    Compile / npmDependencies ++= Seq(
      "libsodium-wrappers" -> "0.7.13",
    )
  )

lazy val crypto = crossProject(/*JSPlatform,*/ JVMPlatform).in(file("Modules/Crypto"))
  .settings(
    scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.munit,
    Dependencies.munitCheck,
  ).jvmSettings(
    Dependencies.bouncyCastle,
    Dependencies.tink
  )

lazy val channels = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Full)
  .in(file("Modules/Channels"))
  .dependsOn(rdts)
  .jvmConfigure(_.dependsOn(crypto.jvm))
  .settings(
    Settings.scala3defaults,
    Settings.javaOutputVersion(17),
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.slips,
    Dependencies.munit,
    SettingsLocal.publishSonatype
  )
  .jsSettings(
    Settings.jsEnvDom,
    Dependencies.scalajsDom,
    Dependencies.scalatags(),
  )
  .jvmSettings(
    Test / fork := true,
    libraryDependencies ++= Dependencies.jetty.map(_ % Provided),
    Dependencies.slf4jSimple,
    Dependencies.sslcontextKickstart
  )

lazy val deltalens = project.in(file("Modules/Deltalens"))
  .dependsOn(rdts.jvm)
  .settings(
    scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.munit,
    Dependencies.scalatest,
  )

lazy val dtn = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full)
  .in(file("Modules/DTN"))
  .dependsOn(reactives, rdts, replication)
  .settings(
    scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.jsoniterScala,
    Dependencies.sttpCore,
    Dependencies.borer
  )

lazy val exampleLenses = project.in(file("Modules/Examples/ReactiveLenses"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(reactives.js)
  .settings(
    scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.scalatags(),
    SettingsLocal.deployTask,
  )

lazy val examplesMiscJVM = project.in(file("Modules/Examples/Misc JVM"))
  .enablePlugins(JmhPlugin)
  .dependsOn(reactives.jvm, replication.jvm, deltalens)
  .settings(
    scala3defaults,
    fork := true,
    Dependencies.jsoniterScala,
    Dependencies.munitCheck,
    Dependencies.tink,
    libraryDependencies += Dependencies.scalafx,
    Dependencies.scalaXml,
    Dependencies.scalaSwing,
    Dependencies.conscript,
    Settings.implicitConversions(), // reswing uses this in a million places for no reason
  )

lazy val loCal = project.in(file("Modules/Examples/Lore Calendar"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(rdts.js, reactives.js, channels.js, lore.js, replication.js)
  .settings(
    scala3defaults,
    Settings.resolverJitpack,
    Dependencies.scalatags(),
    Dependencies.jsoniterScala,
    SettingsLocal.deployTask
  )

lazy val lore = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).in(file("Modules/Lore"))
  .dependsOn(reactives)
  .settings(
    // unstable variant does not enable an inline binary check, because the LoRe DLS has A LOT of private but public members
    Settings.scala3defaultsUnstable,
    Settings.javaOutputVersion(17),
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
    Dependencies.jsoniterScala,
    Dependencies.decline,
    Dependencies.catsParse,
    Dependencies.fansi,
    Dependencies.monocleCore,
    Dependencies.munit,
    Compile / mainClass := Some("lore.Compiler")
  )

lazy val loreCompilerPlugin = project.in(file("Modules/LoRe Compiler Plugin"))
  .dependsOn(lore.jvm)
  .settings(
    scala3defaults,
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
    Dependencies.osLib,
    Dependencies.upickle,
    Dependencies.munit
  )

lazy val loreCompilerPluginExamples = project.in(file("Modules/LoRe Compiler Plugin/examples"))
  .dependsOn(lore.jvm)
  .dependsOn(loreCompilerPlugin)
  .settings(
    scala3defaults,
    Dependencies.munit,
    scalacOptions += {
      val pluginClasspath = (loreCompilerPlugin / Compile / fullClasspathAsJars).value
        .map(at => at.data).mkString(java.io.File.pathSeparator)
      s"-Xplugin:${pluginClasspath}"
    }
  )

lazy val microbenchmarks = project.in(file("Modules/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .dependsOn(reactives.jvm, rdts.jvm, replication.jvm)
  .settings(
    scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.jsoniterScala,
    Settings.jolSettings,
    Dependencies.tink,
    Dependencies.conscript,
  )

lazy val proBench = project.in(file("Modules/Examples/Protocol Benchmarks"))
  .dependsOn(reactives.jvm, rdts.jvm, channels.jvm, rdts.jvm % "compile->compile;test->test", replication.jvm)
  .enablePlugins(JavaAppPackaging)
  .settings(
    scala3defaults,
    Settings.javaOutputVersion(17),
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.jsoniterScala,
    Dependencies.munitCheck,
    Dependencies.munit,
    Dependencies.slips,
    Dependencies.jetcd,
    Dependencies.pprint,
    Universal / packageName := "probench",
    Universal / name        := "probench",
  )

lazy val rdts = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure)
  .in(file("Modules/RDTs"))
  .settings(
    scala3defaults,
    Settings.javaOutputVersion(17),
    Settings.safeInit(Compile / compile),
    Settings.explicitNulls(Compile / compile),
    SettingsLocal.publishSonatype,
    Dependencies.munit,
    Dependencies.munitCheck,
  )
  .jsSettings(
    Settings.sourcemapFromEnv()
  )

lazy val reactives = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("Modules/Reactives"))
  .settings(
    scala3defaults,
    Settings.javaOutputVersion(17),
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    SettingsLocal.publishSonatype,
    Dependencies.munitCheck,
    Dependencies.munit,
  )
  .jvmSettings(
    libraryDependencies += Dependencies.scalafx % Provided,
  )
  .jsSettings(
    Dependencies.scalajsDom,
    Dependencies.scalatags(Test),
    Settings.jsEnvDom,
    Settings.sourcemapFromEnv(),
  )

lazy val replication = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("Modules/Replication"))
  .dependsOn(reactives, rdts, channels, rdts % "compile->compile;test->test")
  .settings(
    scala3defaults,
    Settings.javaOutputVersion(17),
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    SettingsLocal.publishSonatype,
    Dependencies.munitCheck,
    Dependencies.munit,
    Dependencies.jsoniterScala,
  ).jvmSettings(
    Dependencies.tink,
    Dependencies.bouncyCastle,
    Test / fork := true,
  )

lazy val todolist = project.in(file("Modules/Examples/TodoMVC"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(replication.js, dtn.js)
  .settings(
    scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Settings.resolverJitpack,
    Dependencies.scalatags(),
    Dependencies.jsoniterScala,
    SettingsLocal.deployTask,
    Dependencies.pprint,
    scalaJSLinkerConfig := {
      scalaJSLinkerConfig.value
        .withExperimentalUseWebAssembly(true) // use the Wasm backend
        .withModuleKind(ModuleKind.ESModule)  // required by the Wasm backend
    },
    // todolist does not have tests, but still fails to execute them with Wasm backend
    test      := {},
    testQuick := {},
  )

lazy val webview = project.in(file("Modules/Webview"))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(replication.native)
  .settings(
    Settings.scala3defaults,
    Settings.explicitNulls(Compile / compile),
    Settings.safeInit(Compile / compile),
    Dependencies.jsoniterScala,
    FetchResources.fetchedResources += FetchResources.ResourceDescription(
      (Compile / unmanagedResourceDirectories).value.head.toPath.resolve("scala-native/webview.h"),
      URI.create(
        "https://raw.githubusercontent.com/webview/webview/93be13a101e548c13d47ae36a6ea00300b2ecfc0/webview.h"
      ),
      "593cbc6714e5ea1239006991fff0cad55eee02b7"
    ),
    nativeLink := (Compile / nativeLink).dependsOn(FetchResources.fetchResources).value,
    nativeConfig ~= { c =>
      val d = c.withLTO(LTO.thin)
        .withMode(Mode.releaseFast)
        .withIncrementalCompilation(true)
      // The below disables LTO for macos as that seems to cause problems.
      // Windows not implemented, macos has known issues.
      SettingsLocal.osSpecificWebviewConfig(d)
    }
  )
