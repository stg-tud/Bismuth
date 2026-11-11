import org.scalajs.linker.interface.{ESVersion, ModuleInitializer, ModuleSplitStyle}

// for some reason, project matrix REALLY likes to require this everywhere
lazy val s3v = "3.10.0"
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
def scalafx    = "org.scalafx"   %% "scalafx"     % "27.0.0-R39"
def upickle    = "com.lihaoyi"   %% "upickle"     % "4.4.3"

def jsoniterScala = Seq(
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.42.3",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.42.3" % Provided
)

lazy val bismuth = project.in(file(".")).settings(Settings.strictScalacFlags).aggregate(
  channels.js(s3v),
  channels.jvm(s3v),
  examples.js(s3v),
  examples.jvm(s3v),
  lore.js(s3v),
  lore.jvm(s3v),
  loreCompilerPlugin,
  rdts.js(s3v),
  rdts.jvm(s3v),
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
      libraryDependencies += "io.github.hakky54" % "ayza-for-pem" % "10.1.0",
    )
  )
  .jsPlatform(
    scalaVersions = Seq(s3v),
    settings = Seq(
      libraryDependencies ++= Seq(
        scalajsDom,
        "com.lihaoyi"  %% "scalatags"                 % "0.13.1",
        "org.scala-js" %% "scalajs-java-securerandom" % "1.0.0" `cross` CrossVersion.for3Use2_13,
      ),
    )
  )
  .nativePlatform(scalaVersions = Seq(s3v))

/** Case studies and benchmarks. Shared code (e.g., `ex2024textRDT`, imported from
  * https://github.com/mohe2015/bachelor-thesis-collaborative-text-editing) lives in `src/main/scala`,
  * platform-specific case studies in `src/main/scalajvm` and `src/main/scalajs`.
  */
lazy val examples = projectMatrix.in(file("Modules/Examples"))
  .dependsOn(
    reactives,
    rdts,
    lore,
    channels % "compile->compile;test->test",
  )
  .settings(
    publish / skip := true,
    libraryDependencies ++= jsoniterScala,
    libraryDependencies += pprint,
  )
  .jvmPlatform(
    scalaVersions = Seq(s3v),
    axisValues = Nil,
    configure = _.enablePlugins(JmhPlugin).settings(
      Settings.strictScalacFlags,
      // Init-checker warnings positioned in library code, where no local @nowarn reaches:
      // - scala-swing's constructors pass `this` to listeners before subclasses are initialized,
      //   so every subclass of a scala-swing component is flagged
      // - reactive networks defined in constructors that reference fields defined later (cyclic signals) are flagged
      //   inside the inlined reactives macros
      scalacOptions += "-Wconf:msg=initialized|initialization&src=.*(scala/swing|reactives/operator)/.*:s",
      Settings.javaOutputVersion(21),
      fork := true,
      Settings.jolSettings,
      libraryDependencies ++= {
        val jettyVersion = "12.1.14"
        Seq(
          "com.github.alexandrnikitin"     % "bloom-filter_2.13"            % "0.13.1",
          "com.google.crypto.tink"         % "tink"                         % "1.24.0",
          "com.softwaremill.sttp.client4" %% "core"                         % "4.1.1",
          "io.bullet"                     %% "borer-core"                   % "1.19.0",
          "io.bullet"                     %% "borer-derivation"             % "1.19.0",
          "io.etcd"                        % "jetcd-core"                   % "0.8.7",
          "site.ycsb"                      % "core"                         % "0.17.0",
          "org.conscrypt"                  % "conscrypt-openjdk-uber"       % "2.9.0",
          "org.eclipse.jetty.websocket"    % "jetty-websocket-jetty-api"    % jettyVersion,
          "org.eclipse.jetty.websocket"    % "jetty-websocket-jetty-client" % jettyVersion,
          "org.eclipse.jetty.websocket"    % "jetty-websocket-jetty-server" % jettyVersion,
          "org.scala-lang.modules"        %% "scala-swing"                  % "3.0.0",
          "org.scala-lang.modules"        %% "scala-xml"                    % "2.5.0",
          "org.slf4j"                      % "slf4j-nop"                    % "2.0.20" % Test,
          decline,
          scalafx,
          slips,
        )
      },
      javaOptions ++= Seq(
        "-XX:+IgnoreUnrecognizedVMOptions",
        "--sun-misc-unsafe-memory-access=allow",
        "--enable-native-access=ALL-UNNAMED",
        "--enable-final-field-mutation=ALL-UNNAMED"
      ), // Reduce warnings for JavaFX application
    )
  )
  .jsPlatform(
    scalaVersions = Seq(s3v),
    axisValues = Nil,
    configure = _.settings(
      Settings.strictScalacFlags,
      libraryDependencies ++= Seq(
        "com.github.japgolly.scalajs-react" %% "core"                      % "3.0.0",
        "com.github.japgolly.scalajs-react" %% "extra"                     % "3.0.0",
        "com.lihaoyi"                       %% "scalatags"                 % "0.13.1" % Compile,
        "com.olvind"                        %% "scalablytyped-runtime"     % "2.4.2",
        "com.github.cornerman"              %% "colibri-router"            % "0.8.6",
        "io.github.outwatch"                %% "outwatch"                  % "1.1.0",
        "org.scala-js"                      %% "scalajs-java-securerandom" % "1.0.0" `cross` CrossVersion.for3Use2_13,
        scalajsDom,
      ),
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
        (Compile / scalaJSLinkerConfig).value
          .withModuleKind(ModuleKind.CommonJSModule)
          .withModuleSplitStyle(ModuleSplitStyle.FewestModules),
      // let node resolve jsdom and the prosemirror packages from the website node_modules
      Test / jsEnv := Def.uncached {
        new org.scalajs.jsenv.nodejs.NodeJSEnv(
          org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withEnv(
            Map("NODE_PATH" -> ((LocalRootProject / baseDirectory).value / "Website/node_modules").getAbsolutePath)
          )
        )
      },
      // fix the output directory to make it “guessable” by JS import (see main.js)
      // (matrix rows have their baseDirectory under .sbt/matrix, so anchor to the module dir instead)
      Compile / fastLinkJS / scalaJSLinkerOutputDirectory :=
        (LocalRootProject / baseDirectory).value / "Website/target/generated_js/examples-fastopt",
      Compile / fullLinkJS / scalaJSLinkerOutputDirectory :=
        (LocalRootProject / baseDirectory).value / "Website/target/generated_js/examples-opt",
    )
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
      upickle,
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

lazy val scripts = project.in(file("Modules/scripts"))
  .settings(
    libraryDependencies ++= Seq(
      slips,
    ),
    fork                := true,
    run / baseDirectory := (ThisBuild / baseDirectory).value,
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
