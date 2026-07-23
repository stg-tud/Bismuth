import sbt.*
import sbt.Keys.*

object SettingsLocal {

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
