import org.scalajs.linker.interface.{ESVersion, ModuleInitializer, ModuleSplitStyle}

import scala.scalanative.build.{LTO, Mode}

// for some reason, project matrix REALLY likes to require this everywhere
lazy val s3v = "3.9.0"
scalaVersion := s3v
Settings.scala3defaults

// 2026-06-22 scalacheck depends on scala native 0.5.8 while we use 0.5.12; this is likely fine as long as the tests dont fail
libraryDependencySchemes += "org.scala-native" % "test-interface_native0.5_3" % VersionScheme.EarlySemVer

lazy val bismuth = project.in(file(".")).settings(Settings.scala3defaultsExtra).aggregate(
  channels.js(s3v),
  channels.jvm(s3v),
  exJVM,
  exWeb,
  lore.js(s3v),
  lore.jvm(s3v),
  loreCompilerPlugin,
  proBench,
  rdts.js(s3v),
  rdts.jvm(s3v),
  reform,
  rdts.native(s3v),
  reactives.js(s3v),
  reactives.jvm(s3v),
  reactives.native(s3v),
)

lazy val publishedProjects =
  project.in(file("target/PhonyBuilds/publishedProjects")).settings(Settings.scala3defaultsExtra, publish / skip := true)
    .aggregate(
      rdts.jvm(s3v),
      rdts.native(s3v),
      rdts.js(s3v),
      reactives.jvm(s3v),
      reactives.native(s3v),
      reactives.js(s3v),
      channels.jvm(s3v),
      channels.native(s3v),
      channels.js(s3v),
    )
    // set publishing settings to have aggregate commands of bundle uploading work,
    // but do not publish this project itselfs
    .settings(SettingsLocal.publishSonatype, publish / skip := true)

// projects in alphabetical order

lazy val channels = (projectMatrix in file("Modules/Channels"))
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
  .jvmPlatform(
    scalaVersions = Seq(s3v),
    settings = Seq(
      Test / fork := true,
      Dependencies.ayza,
    )
  )
  .jsPlatform(
    scalaVersions = Seq(s3v),
    settings = Seq(
      Dependencies.scalajsDom,
      Dependencies.scalatags(),
    )
  )
  .nativePlatform(scalaVersions = Seq(s3v))

lazy val exJVM = project.in(file("Modules/exJVM"))
  .enablePlugins(JmhPlugin)
  .dependsOn(
    reactives.jvm(s3v),
    channels.jvm(s3v) % "compile->compile;test->test"
  )
  .settings(
    Settings.javaOutputVersion(21),
    fork := true,
    Settings.jolSettings,
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
  .dependsOn(channels.js(s3v), rdts.js(s3v), lore.js(s3v))
  .settings(
    Dependencies.jsoniterScala,
    Dependencies.munit,
    Dependencies.pprint,
    Dependencies.scalajsDom,
    Dependencies.scalajsReact,
    Dependencies.scalatags(),
    Settings.scala3defaultsExtra,
    Compile / scalaJSLinkerConfig :=
      scalaJSLinkerConfig.value
        // WASM does NOT work when running on webview (and is documented to not work on chrome)
        // vite also seems to not really work with WASM – it kinda does in dev mode, but not when bundling
        // also disable module splitting when working with wasm
        .withESFeatures(_.withUseWebAssembly(false))
        .withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("webapps")))
        .withESFeatures(_.withESVersion(ESVersion.ES2015)),
    Test / scalaJSLinkerConfig :=
      scalaJSLinkerConfig.value,
    // fix the output directory to make it “guessable” by JS import
    fastLinkJS / crossTarget := target.value / "generated_js",
    fullLinkJS / crossTarget := target.value / "generated_js",
  )

lazy val lore = (projectMatrix in file("Modules/Lore"))
  .dependsOn(reactives)
  .settings(
    Settings.javaOutputVersion(17),
    libraryDependencies += ("org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided").platform(
      Platform.jvm
    ),
    Dependencies.jsoniterScala,
    Dependencies.decline,
    Dependencies.catsParse,
    Dependencies.fansi,
    Dependencies.monocleCore,
    Dependencies.munit,
    Compile / mainClass := Some("lore.Compiler")
  )
  .jvmPlatform(scalaVersions = Seq(s3v))
  .jsPlatform(scalaVersions = Seq(s3v))

lazy val loreCompilerPlugin = project.in(file("Modules/LoRe Compiler Plugin"))
  .dependsOn(lore.jvm(s3v))
  .settings(
    Settings.javaOutputVersion(17),
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
    Dependencies.upickle,
    Dependencies.munit
  )

lazy val loreCompilerPluginExamples = project.in(file("Modules/LoRe Compiler Plugin/examples"))
  .dependsOn(lore.jvm(s3v))
  .dependsOn(loreCompilerPlugin)
  .settings(
    Settings.javaOutputVersion(17),
    Dependencies.munit,
    scalacOptions += {
      val pluginClasspath = (loreCompilerPlugin / Compile / fullClasspathAsJars).value
        .map(at => at.data).mkString(java.io.File.pathSeparator)
      s"-Xplugin:$pluginClasspath"
    }
  )

lazy val proBench = project.in(file("Modules/Protocol Benchmarks"))
  .dependsOn(
    reactives.jvm(s3v),
    channels.jvm(s3v),
    rdts.jvm(s3v) % "compile->compile;test->test"
  )
  .settings(
    Settings.scala3defaultsExtra,
    Dependencies.jsoniterScala,
    Dependencies.munitCheck,
    Dependencies.munit,
    Dependencies.slips,
    Dependencies.jetcd,
    Dependencies.pprint,
    Dependencies.ycsb,
  )

lazy val rdts = (projectMatrix in file("Modules/RDTs"))
  .settings(
    Settings.scala3defaultsExtra,
    SettingsLocal.publishSonatype,
    Dependencies.munit,
    Dependencies.munitCheck,
  )
  .jvmPlatform(scalaVersions = Seq(s3v))
  .jsPlatform(scalaVersions = Seq(s3v))
  .nativePlatform(scalaVersions = Seq(s3v))

lazy val reactives = (projectMatrix in file("Modules/Reactives"))
  .settings(
    Settings.scala3defaultsExtra,
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    SettingsLocal.publishSonatype,
    Dependencies.munitCheck,
    Dependencies.munit,
  )
  .jvmPlatform(
    scalaVersions = Seq(s3v),
    settings = Seq(
      libraryDependencies += Dependencies.scalafx % Provided,
    )
  )
  .jsPlatform(
    scalaVersions = Seq(s3v),
    settings = Seq(
      Dependencies.scalajsDom,
      Dependencies.scalatags(Test),
    )
  )
  .nativePlatform(
    scalaVersions = Seq(s3v),
  )

lazy val reform = project
  .in(file("Modules/Reform"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(reactives.js(s3v), rdts.js(s3v))
  .settings(
    name := "Reform",
    Dependencies.jsoniterScala,
    Dependencies.munit,
    Compile / scalaJSModuleInitializers := Seq(
      ModuleInitializer.mainMethod("de.tu_darmstadt.informatik.st.reform.Main", "main").withModuleID("main")
    ),
    Test / scalaJSUseTestModuleInitializer := true,
    Test / jsEnv                           := Def.uncached { new org.scalajs.jsenv.nodejs.NodeJSEnv() },
    Test / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    Compile / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)),
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory := target.value / "reform-fastopt",
    Compile / fullLinkJS / scalaJSLinkerOutputDirectory := target.value / "reform-opt",
    libraryDependencies ++= Seq(
      "io.github.outwatch"   %% "outwatch"                  % "1.1.0",
      "com.github.cornerman" %% "colibri-router"            % "0.8.6",
      ("org.scala-js"        %% "scalajs-java-securerandom" % "1.0.0").cross(CrossVersion.for3Use2_13),
    ),
  )

lazy val webview = project.in(file("Modules/Webview"))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(channels.native(s3v))
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
