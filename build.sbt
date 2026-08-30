import org.scalajs.linker.interface.{ESVersion, ModuleInitializer, ModuleSplitStyle}

import scala.scalanative.build.{LTO, Mode, NativeConfig}

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
  project.in(file("target/PhonyBuilds/publishedProjects")).settings(
    Settings.scala3defaultsExtra,
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
    Settings.scala3defaultsExtra,
    slips,
    blake3,
    munit,
    munitCheck,
    jsoniterScala,
    publishSonatype,
  )
  .jvmPlatform(
    scalaVersions = Seq(s3v),
    settings = Seq(
      Test / fork := true,
      ayza,
    )
  )
  .jsPlatform(
    scalaVersions = Seq(s3v),
    settings = Seq(
      scalajsDom,
      scalatags(),
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
    bloomFilter,
    borer,
    conscrypt,
    decline,
    jetty,
    jsoniterScala,
    munit,
    munitCheck,
    pprint,
    scalaSwing,
    scalaXml,
    slf4jnop, // for jetty
    slips,
    sttpCore,
    tink,
    libraryDependencies += scalafx,
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
    jsoniterScala,
    munit,
    pprint,
    scalajsDom,
    scalajsReact,
    scalatags(),
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

lazy val lore = projectMatrix.in(file("Modules/Lore"))
  .dependsOn(reactives)
  .settings(
    Settings.javaOutputVersion(17),
    libraryDependencies += ("org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided").platform(
      Platform.jvm
    ),
    jsoniterScala,
    decline,
    catsParse,
    fansi,
    monocleCore,
    munit,
    Compile / mainClass := Some("lore.Compiler")
  )
  .jvmPlatform(scalaVersions = Seq(s3v))
  .jsPlatform(scalaVersions = Seq(s3v))

lazy val loreCompilerPlugin = project.in(file("Modules/LoRe Compiler Plugin"))
  .dependsOn(lore.jvm(s3v))
  .settings(
    Settings.javaOutputVersion(17),
    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
    upickle,
    munit
  )

lazy val loreCompilerPluginExamples = project.in(file("Modules/LoRe Compiler Plugin/examples"))
  .dependsOn(lore.jvm(s3v))
  .dependsOn(loreCompilerPlugin)
  .settings(
    Settings.javaOutputVersion(17),
    munit,
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
    jsoniterScala,
    munitCheck,
    munit,
    slips,
    jetcd,
    pprint,
    ycsb,
  )

lazy val rdts = projectMatrix.in(file("Modules/RDTs"))
  .settings(
    Settings.scala3defaultsExtra,
    publishSonatype,
    munit,
    munitCheck,
  )
  .jvmPlatform(scalaVersions = Seq(s3v))
  .jsPlatform(scalaVersions = Seq(s3v))
  .nativePlatform(scalaVersions = Seq(s3v))

lazy val reactives = projectMatrix.in(file("Modules/Reactives"))
  .settings(
    Settings.scala3defaultsExtra,
    // scaladoc
    autoAPIMappings := true,
    Compile / doc / scalacOptions += "-groups",
    publishSonatype,
    munitCheck,
    munit,
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
      scalajsDom,
      scalatags(Test),
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
    jsoniterScala,
    munit,
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
    jsoniterScala,
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

//////////// DEPENDENCIES

def akka        = libraryDependencies += "com.typesafe.akka"             %% "akka-actor-typed"         % "2.8.8"
def akkaTestKit = libraryDependencies += "com.typesafe.akka"             %% "akka-actor-testkit-typed" % "2.8.8"  % Test
def ayza        = libraryDependencies += "io.github.hakky54"              % "ayza-for-pem"             % "10.0.6"
def blake3      = libraryDependencies += "pt.kcry"                       %% "blake3"                   % "3.1.2"
def bloomFilter = libraryDependencies += "com.github.alexandrnikitin"     % "bloom-filter_2.13"        % "0.13.1"
def catsParse   = libraryDependencies += "org.typelevel"                 %% "cats-parse"               % "1.1.0"
def conscrypt   = libraryDependencies += "org.conscrypt"                  % "conscrypt-openjdk-uber"   % "2.6.3"
def decline     = libraryDependencies += "com.monovore"                  %% "decline"                  % "2.6.2"
def fansi       = libraryDependencies += "com.lihaoyi"                   %% "fansi"                    % "0.5.1"
def jetcd       = libraryDependencies += "io.etcd"                        % "jetcd-core"               % "0.8.6"
def monocleCore = libraryDependencies += "dev.optics"                    %% "monocle-core"             % "3.3.0"
def munit       = libraryDependencies += "org.scalameta"                 %% "munit"                    % "1.3.5"  % Test
def munitCheck  = libraryDependencies += "org.scalameta"                 %% "munit-scalacheck"         % "1.3.0"  % Test
def pprint      = libraryDependencies += "com.lihaoyi"                   %% "pprint"                   % "0.9.6"
def scalaSwing  = libraryDependencies += "org.scala-lang.modules"        %% "scala-swing"              % "3.0.0"
def scalaXml    = libraryDependencies += "org.scala-lang.modules"        %% "scala-xml"                % "2.4.0"
def scalajsDom  = libraryDependencies += "org.scala-js"                  %% "scalajs-dom"              % "2.8.1"
def slf4jSimple = libraryDependencies += "org.slf4j"                      % "slf4j-simple"             % "2.0.18" % Test
def slf4jnop    = libraryDependencies += "org.slf4j"                      % "slf4j-nop"                % "2.0.18" % Test
def slips       = libraryDependencies += "de.rmgk.slips"                 %% "slips"                    % "0.20.0"
def sttpCore    = libraryDependencies += "com.softwaremill.sttp.client4" %% "core"                     % "4.0.26"
def tink        = libraryDependencies += "com.google.crypto.tink"         % "tink"                     % "1.23.0"
def upickle     = libraryDependencies += "com.lihaoyi"                   %% "upickle"                  % "4.4.3"
def ycsb        = libraryDependencies += "site.ycsb"                      % "core"                     % "0.17.0"

def borer = libraryDependencies ++= Seq(
  "io.bullet" %% "borer-core"       % "1.17.0",
  "io.bullet" %% "borer-derivation" % "1.17.0"
)

def jetty = {
  val jettyVersion = "12.1.12"
  libraryDependencies ++= Seq(
    "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-server" % jettyVersion,
    "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-client" % jettyVersion,
    "org.eclipse.jetty.websocket" % "jetty-websocket-jetty-api"    % jettyVersion,
  )
}

def jsoniterScala =
  libraryDependencies ++= Seq(
    "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.40.1",
    "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.40.1" % Provided
  )

def scalafx: ModuleID = "org.scalafx" %% "scalafx" % "26.0.0-R38"

def scalajsReact = libraryDependencies ++= Seq(
  "com.github.japgolly.scalajs-react" %% "core"  % "3.0.0",
  "com.github.japgolly.scalajs-react" %% "extra" % "3.0.0",
)

def scalatags(conf: Configuration = Compile) = libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.13.1" % conf
