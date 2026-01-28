/* This file is shared between multiple projects
 * and may contain unused dependencies */

// scalajs 1.0
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.20.2")
libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.1"

// scalanative
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.10")

// crossbuilding
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")

// sbt settings
addSbtPlugin("com.github.sbt" % "sbt-dynver"    % "5.1.1") // https://github.com/dwijnand/sbt-dynver
addSbtPlugin("com.eed3si9n"   % "sbt-buildinfo" % "0.13.1")

// packaging
addSbtPlugin("com.github.sbt" % "sbt-pgp"             % "2.3.1")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.11.7")

// tooling
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.5")
