import Settings.{javaOutputVersion, scala3defaults, scala3defaultsExtra}
import org.scalajs.linker.interface.{ESVersion, ModuleSplitStyle}

import scala.scalanative.build.{LTO, Mode}

lazy val bismuth = project.in(file(".")).settings(scala3defaultsExtra).aggregate(
  bft,
  channels.js,
  channels.jvm,
  crypto.js,
  crypto.jvm,
  deltalens,
  dtn.js,
  dtn.jvm,
  examplesJVM,
  examplesWeb,
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
  tabularApp,
  tabularLib,
)

// aggregate projects allow compiling all variants (js, jvm, native) at the same time

lazy val publishedProjects =
  project.in(file("target/PhonyBuilds/publishedProjects")).settings(scala3defaultsExtra, publish / skip := true)
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
    )
    // set publishing settings to have aggregate commands of bundle uploading work,
    // but do not publish this project itselfs
    .settings(SettingsLocal.publishSonatype, publish / skip := true)

// projects in alphabetical order

lazy val bft = project.in(file("Modules/BFT"))
  .dependsOn(
    crypto.jvm
  )
  .settings(
    scala3defaultsExtra,
    Dependencies.munit,
    Dependencies.jsoniterScala,
    Dependencies.bouncyCastle
  )

lazy val channels = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Full)
  .in(file("Modules/Channels"))
  .dependsOn(rdts)
  .settings(
    Settings.scala3defaultsExtra,
    Dependencies.slips,
    Dependencies.munit,
    Dependencies.munitCheck,
    Dependencies.jsoniterScala,
    SettingsLocal.publishSonatype,
  )
  .jsSettings(
    Settings.jsEnvDom,
    Dependencies.scalajsDom,
    Dependencies.scalatags(),
  )
  .jvmSettings(
    Test / fork := true,
  )

lazy val crypto = crossProject(JSPlatform, JVMPlatform).in(file("Modules/Crypto"))
  .dependsOn(
    rdts,
    channels % "compile->compile;test->test",
  )
  .settings(
    scala3defaultsExtra,
    Dependencies.munit,
    Dependencies.munitCheck,
    Dependencies.slips,
    Dependencies.jsoniterScala,
  )
  .jvmSettings(
    Dependencies.bouncyCastle,
    Dependencies.sslcontextKickstart,
    Dependencies.tink,
  ).jsSettings(
    // commonjs module allows tests to find libsodium-wrappers installed in the root project
    Test / scalaJSLinkerConfig := {
      (Test / scalaJSLinkerConfig).value
        .withModuleKind(ModuleKind.CommonJSModule)
    },
  )

lazy val deltalens = project.in(file("Modules/Deltalens"))
  .dependsOn(rdts.jvm)
  .settings(
    scala3defaultsExtra,
    Dependencies.munit,
    Dependencies.scalatest,
  )

lazy val dtn = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full)
  .in(file("Modules/DTN"))
  .dependsOn(reactives, rdts, channels)
  .settings(
    scala3defaultsExtra,
    Dependencies.jsoniterScala,
    Dependencies.sttpCore,
    Dependencies.borer
  )

lazy val examplesJVM = project.in(file("Modules/Examples JVM"))
  .enablePlugins(JmhPlugin)
  .dependsOn(deltalens, reactives.jvm, crypto.jvm, channels.jvm % "compile->compile;test->test")
  .settings(
    scala3defaults,
    javaOutputVersion(17),
    fork := true,
    Dependencies.conscript,
    Dependencies.jetty,
    Dependencies.jsoniterScala,
    Dependencies.munitCheck,
    Dependencies.pprint,
    Dependencies.scalaSwing,
    Dependencies.scalaXml,
    Dependencies.slf4jnop, // for jetty
    Dependencies.slips,
    Dependencies.tink,
    libraryDependencies += Dependencies.scalafx,
    Settings.implicitConversions(), // reswing uses this in a million places for no reason
  )

lazy val examplesWeb = project.in(file("Modules/Examples Web"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(dtn.js, lore.js)
  .settings(
    scala3defaultsExtra,
    Dependencies.scalatags(),
    Dependencies.jsoniterScala,
    Dependencies.pprint,
    scalaJSLinkerConfig := {
      scalaJSLinkerConfig.value
        // WASM does NOT work when running on webview (and is documented to not work on chrome)
        // vite also seems to not really work with WASM – it kinda does in dev mode, but not when bundling
        // also disable module splitting when working with wasm
        .withExperimentalUseWebAssembly(false)
        .withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("webapps")))
        .withESFeatures(_.withESVersion(ESVersion.ES2015))
    },
    // fix the output directory to make it “guessable” by JS import
    fastLinkJS / crossTarget := target.value / "generated_js",
    fullLinkJS / crossTarget := target.value / "generated_js",
    // examples do not have tests, but still fail to execute them with WASM backend
    test      := {},
    testQuick := {},
  )

lazy val lore = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).in(file("Modules/Lore"))
  .dependsOn(reactives)
  .settings(
    scala3defaults,
    javaOutputVersion(17),
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
    javaOutputVersion(17),
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
    javaOutputVersion(17),
    Dependencies.munit,
    scalacOptions += {
      val pluginClasspath = (loreCompilerPlugin / Compile / fullClasspathAsJars).value
        .map(at => at.data).mkString(java.io.File.pathSeparator)
      s"-Xplugin:${pluginClasspath}"
    }
  )

lazy val microbenchmarks = project.in(file("Modules/Microbenchmarks"))
  .enablePlugins(JmhPlugin)
  .dependsOn(rdts.jvm, reactives.jvm, channels.jvm, crypto.jvm)
  .settings(
    scala3defaultsExtra,
    Dependencies.jsoniterScala,
    Settings.jolSettings,
    Dependencies.tink,
    Dependencies.conscript,
  )

lazy val proBench = project.in(file("Modules/Protocol Benchmarks"))
  .dependsOn(reactives.jvm, rdts.jvm, channels.jvm, rdts.jvm % "compile->compile;test->test")
  .enablePlugins(JavaAppPackaging)
  .settings(
    scala3defaultsExtra,
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
    scala3defaultsExtra,
    SettingsLocal.publishSonatype,
    Dependencies.munit,
    Dependencies.munitCheck,
  )

lazy val reactives = crossProject(JVMPlatform, JSPlatform, NativePlatform).in(file("Modules/Reactives"))
  .settings(
    scala3defaultsExtra,
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
  )

lazy val webview = project.in(file("Modules/Webview"))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(channels.native)
  .settings(
    Settings.scala3defaultsExtra,
    Dependencies.jsoniterScala,
    nativeConfig ~= { c =>
      val d = c.withLTO(LTO.thin)
        .withMode(Mode.releaseFast)
        .withIncrementalCompilation(true)
      // The below disables LTO for macos as that seems to cause problems.
      // Windows not implemented, macos has known issues.
      SettingsLocal.osSpecificWebviewConfig(d)
    }
  )

lazy val tabularApp = project.in(file("Modules/Tabular/app"))
  .dependsOn(tabularLib)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scala3defaultsExtra,
    Dependencies.scalajsDom,
    libraryDependencies ++= Seq(
      "com.github.japgolly.scalajs-react" %%% "core"  % "2.1.3",
      "com.github.japgolly.scalajs-react" %%% "extra" % "2.1.3",
    ),
    scalaJSUseMainModuleInitializer   := true,
    Compile / fastOptJS / crossTarget := target.value,
    Compile / fullOptJS / crossTarget := target.value
  )

lazy val tabularLib = project.in(file("Modules/Tabular/lib"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(channels.js, rdts.js)
  .settings(
    scala3defaultsExtra,
    Dependencies.munit,
    Dependencies.pprint,
  )
