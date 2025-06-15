import sbt.*
import sbt.Keys.*

import scala.scalanative.build.{LTO, NativeConfig}

object SettingsLocal {

  def osSpecificWebviewConfig(nativeConfig: NativeConfig): NativeConfig = {

    def fromCommand(args: String*): List[String] = {
      val process = new ProcessBuilder(args*).start()
      process.waitFor()
      val res = new String(process.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
      if (process.exitValue() != 0) throw new IllegalStateException(s"command failed: ${args.mkString(" ")}\n$res")
      res.split(raw"\s+").toList
    }

    val osname = sys.props.get("os.name").map(_.toLowerCase)
    osname match {
      case Some(win) if win.contains("win")                           => nativeConfig
      case Some(mac) if mac.contains("mac") || mac.contains("darwin") =>
        nativeConfig.withLTO(LTO.none)
          .withLinkingOptions(nativeConfig.linkingOptions ++ Seq("-framework", "WebKit"))
          .withCompileOptions(co => co ++ Seq("-framework", "WebKit"))
      case Some(linux) if linux.contains("linux") =>
        nativeConfig
          .withLinkingOptions(
            // unfortunately gtk4 version does not work in podman :(
            // nativeConfig.linkingOptions ++ fromCommand("pkg-config", "--libs", "gtk4", "webkitgtk-6.0")
            nativeConfig.linkingOptions ++ fromCommand("pkg-config", "--libs", "gtk+-3.0", "webkit2gtk-4.1")
          )
          // .withCompileOptions(co => co ++ fromCommand("pkg-config", "--cflags", "gtk4", "webkitgtk-6.0"))
          .withCompileOptions(co => co ++ fromCommand("pkg-config", "--cflags", "gtk+-3.0", "webkit2gtk-4.1"))
      case other =>
        println(s"unknown OS: $other")
        nativeConfig
    }
  }

  // publishSigned: to generate bundle to be published into a local staging repo
  // sonaUpload: upload to sonatype and publish and verify manually
  // sonaRelease: to (upload?) and release the bundle automatically
  val publishSonatype = Def.settings(
    organization         := "de.tu-darmstadt.stg",
    organizationName     := "Software Technology Group",
    organizationHomepage := Some(url("https://www.stg.tu-darmstadt.de/")),
    homepage             := Some(url("https://github.com/stg-tud/Bismuth")),
    licenses             := List("Apache 2" -> new URI("http://www.apache.org/licenses/LICENSE-2.0.txt").toURL),
    scmInfo              := Some(
      ScmInfo(
        url("https://github.com/stg-tud/Bismuth"),
        "scm:git@github.com:stg-tud/Bismuth.git"
      )
    ),
    developers := List(
      Developer(
        id = "ragnar",
        name = "Ragnar Mogk",
        email = "mogk@cs.tu-darmstadt.de",
        url = url("https://www.stg.tu-darmstadt.de/")
      )
    ),

    // no binary compatibility for 0.Y.z releases
    versionScheme := Some("semver-spec"),

    // sonatype sbt plugin settings
    // sonatypeCredentialHost := sonatypeCentralHost,
    // sonatypeProfileName    := "de.tu-darmstadt.stg",

    // Remove all additional repository other than Maven Central from POM
    pomIncludeRepository := { _ => false },
    // change to sonatypePublishTo to not use the bundle feature
    publishTo         := localStaging.value,
    publishMavenStyle := true
  )

  // old publishing documentation for legacy purposes
  // use `publishSigned` to publish
  // go to https://oss.sonatype.org/#stagingRepositories to move from staging to maven central
  // alternatively, use `sonatypeRelease` to release from sbt
  // if the bundle feature is enabled, then `publishSigned` only puts the files into a local folder,
  // use `sonatypeBundleRelease` to release that local bundle, or try the individual steps to see intermediate results
  // `sonatypePrepare; sonatypeBundleUpload; sonatypeRelease`

}
