/* This file is shared between multiple projects
 * and may contain unused dependencies */

// scalajs 1.0
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")

// scalanative
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")

// sbt settings
addSbtPlugin("com.github.sbt" % "sbt-dynver"    % "5.1.1") // https://github.com/dwijnand/sbt-dynver
addSbtPlugin("com.eed3si9n"   % "sbt-buildinfo" % "0.13.1")

// packaging
addSbtPlugin("com.github.sbt" % "sbt-pgp"             % "2.3.1")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.11.7")

// tooling
addSbtPlugin("pl.project13.scala" % "sbt-jmh"      % "0.4.8")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix" % "0.14.7")

// jar creation
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.3.1")

// https://github.com/rtimush/sbt-updates
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.7.0")
