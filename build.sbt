import Settings.{javaOutputVersion, scala3VersionString, scala3defaults, scala3defaultsExtra}
import org.scalajs.linker.interface.{ESVersion, ModuleInitializer, ModuleSplitStyle}

import scala.scalanative.build.{LTO, Mode}

resolvers += "Sonatype Snapshots" at "https://central.sonatype.com/repository/maven-snapshots"

evictionErrorLevel := Level.Info

lazy val publishedProjects =
  project.in(file("target/PhonyBuilds/publishedProjects")).settings(scala3defaultsExtra, publish / skip := true)
    .aggregate(
      rdts.jvm(scala3VersionString),
      rdts.native(scala3VersionString),
      reactives.jvm(scala3VersionString),
      reactives.native(scala3VersionString),
      channels.jvm(scala3VersionString),
      channels.native(scala3VersionString),
    )
    // set publishing settings to have aggregate commands of bundle uploading work,
    // but do not publish this project itselfs
    .settings(SettingsLocal.publishSonatype, publish / skip := true)

// projects in alphabetical order

lazy val channels = (projectMatrix in file("Modules/Channels"))
  .dependsOn(rdts)
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
    scalaVersions = Settings.scalaVersions,
    settings = Seq(
      Test / fork := true,
      Dependencies.ayza,
    )
  )
  .jsPlatform(
    scalaVersions = Settings.scalaVersions,
    settings = Seq(
      Dependencies.scalajsDom,
      Dependencies.scalatags(),
    )
  )
  .nativePlatform(scalaVersions = Settings.scalaVersions)

lazy val deltalens = project.in(file("Modules/Deltalens"))
  .dependsOn(rdts.jvm(scala3VersionString))
  .settings(
    scala3defaultsExtra,
    Dependencies.munit,
    Dependencies.scalatest,
  )

lazy val exJVM = project.in(file("Modules/exJVM"))
  .enablePlugins(JmhPlugin)
  .dependsOn(deltalens, reactives.jvm(scala3VersionString), channels.jvm(scala3VersionString) % "compile->compile;test->test")
  .settings(
    scala3defaults,
    javaOutputVersion(21),
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
  .dependsOn(channels.js(scala3VersionString), rdts.js(scala3VersionString), lore.js(scala3VersionString))
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
        .withESFeatures(_.withUseWebAssembly(false))
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

lazy val lore = (projectMatrix in file("Modules/Lore"))
  .dependsOn(reactives)
  .settings(
    scala3defaults,
    javaOutputVersion(17),
    libraryDependencies += ("org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided").platform(Platform.jvm),
    Dependencies.jsoniterScala,
    Dependencies.decline,
    Dependencies.catsParse,
    Dependencies.fansi,
    Dependencies.monocleCore,
    Dependencies.munit,
    Compile / mainClass := Some("lore.Compiler")
  )
  .jvmPlatform(scalaVersions = Settings.scalaVersions)
  .jsPlatform(scalaVersions = Settings.scalaVersions)

lazy val loreCompilerPlugin = project.in(file("Modules/LoRe Compiler Plugin"))
  .dependsOn(lore.jvm(scala3VersionString))
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
  .dependsOn(reactives.jvm(scala3VersionString), rdts.jvm(scala3VersionString), channels.jvm(scala3VersionString), rdts.jvm(scala3VersionString) % "compile->compile;test->test")
  .settings(
    scala3defaultsExtra,
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
    scala3defaultsExtra,
    SettingsLocal.publishSonatype,
    Dependencies.munit,
    Dependencies.munitCheck,
  )
  .jvmPlatform(scalaVersions = Settings.scalaVersions)
  .jsPlatform(scalaVersions = Settings.scalaVersions)
  .nativePlatform(scalaVersions = Settings.scalaVersions)

lazy val reactives = (projectMatrix in file("Modules/Reactives"))
  .settings(
    scala3defaultsExtra,
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    SettingsLocal.publishSonatype,
    Dependencies.munitCheck,
    Dependencies.munit,
  )
  .jvmPlatform(
    scalaVersions = Settings.scalaVersions,
    settings = Seq(
      libraryDependencies += Dependencies.scalafx % Provided,
    )
  )
  .jsPlatform(
    scalaVersions = Settings.scalaVersions,
    settings = Seq(
      Dependencies.scalajsDom,
      Dependencies.scalatags(Test),
    )
  )
  .nativePlatform(
    scalaVersions = Settings.scalaVersions,
  )

lazy val reform = project
  .in(file("Modules/Reform"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(reactives.js(scala3VersionString), rdts.js(scala3VersionString))
  .settings(
    scala3defaults,
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
  .dependsOn(channels.native(scala3VersionString))
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
