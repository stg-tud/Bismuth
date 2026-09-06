import org.scalajs.linker.interface.{ESVersion, ModuleInitializer, ModuleSplitStyle}

import scala.scalanative.build.{LTO, Mode, NativeConfig}

// for some reason, project matrix REALLY likes to require this everywhere
lazy val s3v = "3.9.0"
scalaVersion := s3v
Settings.defaultScalacFlags

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit"            % "1.3.6" % Test,
  "org.scalameta" %% "munit-scalacheck" % "1.3.1" % Test,
)

def decline    = "com.monovore"  %% "decline"     % "2.6.2"
def pprint     = "com.lihaoyi"   %% "pprint"      % "0.9.6"
def scalajsDom = "org.scala-js"  %% "scalajs-dom" % "2.8.1"
def slips      = "de.rmgk.slips" %% "slips"       % "0.20.0"
def scalafx    = "org.scalafx"   %% "scalafx"     % "26.0.0-R38"

def jsoniterScala = Seq(
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.40.1",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.40.1" % Provided
)

lazy val bismuth = project.in(file(".")).settings(Settings.strictScalacFlags).aggregate(
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
  project.in(file("target/PhonyBuilds/publishedProjects")).settings(
    Settings.strictScalacFlags,
    publish / skip := true
  )
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
    // but do not publish this project itself
    .settings(publishSonatype, publish / skip := true)

// projects in alphabetical order

lazy val channels = projectMatrix.in(file("Modules/Channels"))
  .dependsOn(rdts % "compile->compile;test->test")
  .settings(
    Settings.strictScalacFlags,
    publishSonatype,
    libraryDependencies ++= jsoniterScala,
    libraryDependencies ++= Seq(
      "pt.kcry" %% "blake3" % "3.1.2",
      slips,
    ),
  )
  .jvmPlatform(
    scalaVersions = Seq(s3v),
    settings = Seq(
      Test / fork                               := true,
      libraryDependencies += "io.github.hakky54" % "ayza-for-pem" % "10.0.7",
    )
  )
  .jsPlatform(
    scalaVersions = Seq(s3v),
    settings = Seq(
      libraryDependencies ++= Seq(
        scalajsDom,
        "com.lihaoyi" %% "scalatags" % "0.13.1",
      ),
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
    libraryDependencies ++= jsoniterScala,
    libraryDependencies ++= {
      val jettyVersion = "12.1.12"
      Seq(
        "com.github.alexandrnikitin"     % "bloom-filter_2.13"            % "0.13.1",
        "com.google.crypto.tink"         % "tink"                         % "1.23.0",
        "com.softwaremill.sttp.client4" %% "core"                         % "4.0.26",
        "io.bullet"                     %% "borer-core"                   % "1.18.0",
        "io.bullet"                     %% "borer-derivation"             % "1.18.0",
        "org.conscrypt"                  % "conscrypt-openjdk-uber"       % "2.7.0",
        "org.eclipse.jetty.websocket"    % "jetty-websocket-jetty-api"    % jettyVersion,
        "org.eclipse.jetty.websocket"    % "jetty-websocket-jetty-client" % jettyVersion,
        "org.eclipse.jetty.websocket"    % "jetty-websocket-jetty-server" % jettyVersion,
        "org.scala-lang.modules"        %% "scala-swing"                  % "3.0.0",
        "org.scala-lang.modules"        %% "scala-xml"                    % "2.4.0",
        "org.slf4j"                      % "slf4j-nop"                    % "2.0.19" % Test,
        decline,
        pprint,
        scalafx,
        slips,
      )
    },
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
    libraryDependencies ++= Seq(
      "com.github.japgolly.scalajs-react" %% "core"      % "3.0.0",
      "com.github.japgolly.scalajs-react" %% "extra"     % "3.0.0",
      "com.lihaoyi"                       %% "scalatags" % "0.13.1" % Compile,
      pprint,
      scalajsDom,
    ) ++ jsoniterScala,
    Settings.strictScalacFlags,
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
    // (in sbt 2.0 target.value is centralized under target/out, so anchor to the module dir instead)
    fastLinkJS / crossTarget := baseDirectory.value / "target" / "generated_js",
    fullLinkJS / crossTarget := baseDirectory.value / "target" / "generated_js",
  )

lazy val lore = projectMatrix.in(file("Modules/Lore"))
  .dependsOn(reactives)
  .settings(
    Settings.javaOutputVersion(17),
    libraryDependencies ++= jsoniterScala,
    libraryDependencies ++= Seq(
      "com.lihaoyi"    %% "fansi"           % "0.5.1",
      "dev.optics"     %% "monocle-core"    % "3.3.0",
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided" `platform` Platform.jvm,
      "org.typelevel"  %% "cats-parse"      % "1.1.0",
      decline,
    ),
    Compile / mainClass := Some("lore.Compiler")
  )
  .jvmPlatform(scalaVersions = Seq(s3v))
  .jsPlatform(scalaVersions = Seq(s3v))

lazy val loreCompilerPlugin = project.in(file("Modules/LoRe Compiler Plugin"))
  .dependsOn(lore.jvm(s3v))
  .settings(
    Settings.javaOutputVersion(17),
    libraryDependencies ++= Seq(
      "com.lihaoyi"    %% "upickle"         % "4.4.3",
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
    ),
  )

lazy val loreCompilerPluginExamples = project.in(file("Modules/LoRe Compiler Plugin/examples"))
  .dependsOn(lore.jvm(s3v))
  .dependsOn(loreCompilerPlugin)
  .settings(
    Settings.javaOutputVersion(17),
    scalacOptions += {
      val converter       = fileConverter.value
      val pluginClasspath = (loreCompilerPlugin / Compile / fullClasspathAsJars).value
        .map(at => converter.toPath(at.data).toAbsolutePath.toString)
        .mkString(java.io.File.pathSeparator)
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
    Settings.strictScalacFlags,
    libraryDependencies ++= jsoniterScala,
    libraryDependencies ++= Seq(
      "io.etcd"   % "jetcd-core" % "0.8.7",
      "site.ycsb" % "core"       % "0.17.0",
      pprint,
      slips,
    ),
  )

lazy val rdts = projectMatrix.in(file("Modules/RDTs"))
  .settings(
    Settings.strictScalacFlags,
    publishSonatype,
  )
  .jvmPlatform(scalaVersions = Seq(s3v))
  .jsPlatform(scalaVersions = Seq(s3v))
  .nativePlatform(scalaVersions = Seq(s3v))

lazy val reactives = projectMatrix.in(file("Modules/Reactives"))
  .settings(
    Settings.strictScalacFlags,
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    publishSonatype,
  )
  .jvmPlatform(
    scalaVersions = Seq(s3v),
    settings = Seq(
      libraryDependencies += scalafx % Provided,
    )
  )
  .jsPlatform(
    scalaVersions = Seq(s3v),
    settings = Seq(
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "scalatags" % "0.13.1" % Test,
        scalajsDom,
      ),
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
    name                                := "Reform",
    Compile / scalaJSModuleInitializers := Seq(
      ModuleInitializer.mainMethod("de.tu_darmstadt.informatik.st.reform.Main", "main").withModuleID("main")
    ),
    Test / scalaJSUseTestModuleInitializer := true,
    Test / jsEnv                           := Def.uncached { new org.scalajs.jsenv.nodejs.NodeJSEnv() },
    Test / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    Compile / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)),
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory := target.value / "reform-fastopt",
    Compile / fullLinkJS / scalaJSLinkerOutputDirectory := target.value / "reform-opt",
    libraryDependencies ++= jsoniterScala,
    libraryDependencies ++= Seq(
      "com.github.cornerman" %% "colibri-router"            % "0.8.6",
      "io.github.outwatch"   %% "outwatch"                  % "1.1.0",
      "org.scala-js"         %% "scalajs-java-securerandom" % "1.0.0" `cross` CrossVersion.for3Use2_13,
    ),
  )

lazy val webview = project.in(file("Modules/Webview"))
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(channels.native(s3v))
  .settings(
    Settings.strictScalacFlags,
    libraryDependencies ++= jsoniterScala,
    nativeConfig ~= { c =>
      val d = c.withLTO(LTO.thin)
        .withMode(Mode.releaseFast)
        .withIncrementalCompilation(true)

      // The below disables LTO for macos as that seems to cause problems.
      // Windows not implemented, macos has known issues.
      def fromCommand(args: String*): List[String] = {
        val process = new ProcessBuilder(args *).start()
        process.waitFor()
        val res = new String(process.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
        if (process.exitValue() != 0)
          throw new IllegalStateException(s"command failed: ${args.mkString(" ")}\n$res")
        res.split(raw"\s+").toList
      }

      val osname = sys.props.get("os.name").map(_.toLowerCase)
      osname match {
        case Some(win) if win.contains("win")                           => d
        case Some(mac) if mac.contains("mac") || mac.contains("darwin") =>
          d.withLTO(LTO.none)
            .withLinkingOptions(d.linkingOptions ++ Seq("-framework", "WebKit"))
            .withCompileOptions(co => co ++ Seq("-framework", "WebKit"))
        case Some(linux) if linux.contains("linux") =>
          d
            .withLinkingOptions(
              // unfortunately gtk4 version does not work in podman :(
              // nativeConfig.linkingOptions ++ fromCommand("pkg-config", "--libs", "gtk4", "webkitgtk-6.0")
              d.linkingOptions ++ fromCommand("pkg-config", "--libs", "gtk+-3.0", "webkit2gtk-4.1")
            )
            // .withCompileOptions(co => co ++ fromCommand("pkg-config", "--cflags", "gtk4", "webkitgtk-6.0"))
            .withCompileOptions(co => co ++ fromCommand("pkg-config", "--cflags", "gtk+-3.0", "webkit2gtk-4.1"))
        case other =>
          println(s"unknown OS: $other")
          d
      }

    }
  )

////////////////// PUBLISHING SETTINGS

// publishSigned: to generate bundle to be published into a local staging repo
// sonaUpload: upload to sonatype and publish and verify manually
// sonaRelease: to (upload?) and release the bundle automatically
val publishSonatype = Def.settings(
  organization         := "de.tu-darmstadt.stg",
  organizationName     := "Software Technology Group",
  organizationHomepage := Some(uri("https://www.stg.tu-darmstadt.de/")),
  homepage             := Some(uri("https://github.com/stg-tud/Bismuth")),
  licenses             := List(sbt.librarymanagement.License(
    "Apache 2",
    new URI("http://www.apache.org/licenses/LICENSE-2.0.txt")
  )),
  scmInfo := Some(
    ScmInfo(
      uri("https://github.com/stg-tud/Bismuth"),
      "scm:git@github.com:stg-tud/Bismuth.git"
    )
  ),
  developers := List(
    Developer(
      id = "ragnar",
      name = "Ragnar Mogk",
      email = "mogk@cs.tu-darmstadt.de",
      url = uri("https://www.stg.tu-darmstadt.de/")
    )
  ),

  // no binary compatibility for 0.Y.z releases
  versionScheme := Some("semver-spec"),

  // Remove all additional repository other than Maven Central from POM
  pomIncludeRepository := { _ => false },
  // change to sonatypePublishTo to not use the bundle feature
  publishTo         := localStaging.value,
  publishMavenStyle := true
)
