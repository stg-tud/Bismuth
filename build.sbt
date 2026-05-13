import Settings.{javaOutputVersion, scala3defaults, scala3defaultsExtra}
import org.scalajs.linker.interface.{ESVersion, ModuleInitializer, ModuleSplitStyle}

import scala.scalanative.build.{LTO, Mode}

lazy val bismuth = project.in(file(".")).settings(scala3defaultsExtra).aggregate(
  channels.js,
  channels.jvm,
  deltalens,
  exJVM,
  exWeb,
  lore.js,
  lore.jvm,
  loreCompilerPlugin,
  proBench,
  rdts.js,
  rdts.jvm,
  reform.js,
  reform.jvm,
  rdts.native,
  reactives.js,
  reactives.jvm,
  reactives.native,
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

lazy val channels = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Full)
  .in(file("Modules/Channels"))
  .dependsOn(rdts % "compile->compile;test->test")
  .settings(
    Settings.scala3defaultsExtra,
    Dependencies.slips,
    Dependencies.blake3,
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
    Dependencies.ayza,
  )

lazy val deltalens = project.in(file("Modules/Deltalens"))
  .dependsOn(rdts.jvm)
  .settings(
    scala3defaultsExtra,
    Dependencies.munit,
    Dependencies.scalatest,
  )

lazy val exJVM = project.in(file("Modules/exJVM"))
  .enablePlugins(JmhPlugin)
  .dependsOn(deltalens, reactives.jvm, channels.jvm % "compile->compile;test->test")
  .settings(
    scala3defaults,
    javaOutputVersion(21),
    fork := true,
    Settings.jolSettings,
    Dependencies.akka,
    Dependencies.akkaTestKit,
    Dependencies.bloomFilter,
    Dependencies.borer,
    Dependencies.conscrypt,
    Dependencies.decline,
    Dependencies.jetty,
    Dependencies.jsoniterScala,
    Dependencies.munit,
    Dependencies.munitCheck,
    Dependencies.pprint,
    Dependencies.scalaSwing,
    Dependencies.scalaXml,
    Dependencies.slf4jnop, // for jetty
    Dependencies.slips,
    Dependencies.sttpCore,
    Dependencies.tink,
    libraryDependencies += Dependencies.scalafx,
    Settings.implicitConversions(), // reswing uses this in a million places for no reason
    javaOptions ++= Seq(
      "-XX:+IgnoreUnrecognizedVMOptions",
      "--sun-misc-unsafe-memory-access=allow",
      "--enable-native-access=ALL-UNNAMED"
    ), // Reduce warnings for JavaFX application
  )

lazy val exWeb = project.in(file("Modules/exWeb"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(channels.js, rdts.js, lore.js)
  .settings(
    Dependencies.jsoniterScala,
    Dependencies.munit,
    Dependencies.pprint,
    Dependencies.scalajsDom,
    Dependencies.scalajsReact,
    Dependencies.scalatags(),
    scala3defaultsExtra,
    Compile / scalaJSLinkerConfig := {
      scalaJSLinkerConfig.value
        // WASM does NOT work when running on webview (and is documented to not work on chrome)
        // vite also seems to not really work with WASM – it kinda does in dev mode, but not when bundling
        // also disable module splitting when working with wasm
        .withExperimentalUseWebAssembly(false)
        .withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("webapps")))
        .withESFeatures(_.withESVersion(ESVersion.ES2015))
    },
    Test / scalaJSLinkerConfig :=
      scalaJSLinkerConfig.value,
    // fix the output directory to make it “guessable” by JS import
    fastLinkJS / crossTarget := target.value / "generated_js",
    fullLinkJS / crossTarget := target.value / "generated_js",
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
      s"-Xplugin:$pluginClasspath"
    }
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
    Dependencies.ycsb,
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

lazy val reform = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full)
  .in(file("Modules/Reform/peer/src"))
  .dependsOn(reactives, rdts)
  .settings(
    scala3defaults,
    name := "Reform",
    resolvers += "jitpack".at("https://jitpack.io"),
    Dependencies.jsoniterScala,
    Dependencies.munit,
    libraryDependencies += "com.github.scala-loci.scala-loci" %%% "scala-loci-serializer-jsoniter-scala" % "3ea9afdeac1c46b5da65497b7d1fa54152128c2a",
  )
  .jsSettings(
    Compile / scalaJSModuleInitializers := Seq(
      ModuleInitializer.mainMethod("de.tu_darmstadt.informatik.st.reform.Main", "main").withModuleID("main")
    ),
    Test / scalaJSUseTestModuleInitializer := true,
    Test / jsEnv                           := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    Test / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    Compile / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)),
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory := target.value / "reform-fastopt",
    Compile / fullLinkJS / scalaJSLinkerOutputDirectory := target.value / "reform-opt",
    libraryDependencies ++= Seq(
      "io.github.outwatch"   %%% "outwatch"       % "1.0.0-RC14",
      "com.github.cornerman" %%% "colibri-router" % "0.7.8",
      "com.github.scala-loci.scala-loci" %%% "scala-loci-communicator-ws-webnative" % "3ea9afdeac1c46b5da65497b7d1fa54152128c2a",
      "com.github.scala-loci.scala-loci" %%% "scala-loci-communicator-webrtc" % "3ea9afdeac1c46b5da65497b7d1fa54152128c2a",
      "com.github.scala-loci.scala-loci" %%% "scala-loci-communicator-broadcastchannel" % "3ea9afdeac1c46b5da65497b7d1fa54152128c2a",
    ),
  )
  .jvmSettings(
    fork                := true,
    run / baseDirectory := file("Modules/Reform/peer"),
    libraryDependencies ++= Seq(
      "com.github.scala-loci.scala-loci" %%% "scala-loci-communicator-ws-jetty11" % "3ea9afdeac1c46b5da65497b7d1fa54152128c2a",
      "org.eclipse.jetty" % "jetty-slf4j-impl" % "11.0.14",
      "org.xerial"        % "sqlite-jdbc"      % "3.41.0.0",
      "com.auth0"         % "java-jwt"         % "4.3.0",
    ),
    assembly / mainClass := Some("de.tu_darmstadt.informatik.st.reform.Main")
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
