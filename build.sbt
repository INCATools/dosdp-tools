enablePlugins(JavaAppPackaging)

organization  := "org.monarchinitiative"

name          := "dosdp-scala"

version       := "0.0.1-SNAPSHOT"

scalaVersion  := "2.11.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers += Resolver.bintrayRepo("jeremyrsmith", "maven")

javaOptions += "-Xmx8G"

libraryDependencies ++= {
    Seq(
      "net.sourceforge.owlapi" %  "owlapi-distribution"    % "4.2.1",
      "org.phenoscape"         %% "scowl"                  % "1.1",
      "io.circe"               %% "circe-core"             % "0.5.0-M2",
      "io.circe"               %% "circe-generic"          % "0.5.0-M2",
      "io.circe"               %% "circe-parser"           % "0.5.0-M2",
      "io.github.jeremyrsmith" %% "circe-yaml"             % "0.2.1",
      "org.scalatest"          %% "scalatest"              % "2.2.6" % Test
    )
}
