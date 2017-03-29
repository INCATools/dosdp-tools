enablePlugins(JavaAppPackaging)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

organization  := "org.monarchinitiative"

name          := "dosdp-scala"

version       := "0.3-SNAPSHOT"

scalaVersion  := "2.11.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

mainClass in Compile := Some("org.monarchinitiative.dosdp.Main")

javaOptions += "-Xmx8G"

libraryDependencies ++= {
    Seq(
      "net.sourceforge.owlapi"     %  "owlapi-distribution" % "4.2.1",
      "org.phenoscape"             %% "scowl"               % "1.3",
      "org.phenoscape"             %% "owlet"               % "1.5" exclude("org.slf4j", "slf4j-log4j12"),
      "org.semanticweb.elk"        %  "elk-owlapi"          % "0.4.3",
      "io.circe"                   %% "circe-core"          % "0.7.0",
      "io.circe"                   %% "circe-generic"       % "0.7.0",
      "io.circe"                   %% "circe-parser"        % "0.7.0",
      "io.circe"                   %% "circe-yaml"          % "0.4.0",
      "org.apache.jena"            %  "apache-jena-libs"    % "3.2.0" exclude("org.slf4j", "slf4j-log4j12"),
      "org.backuity.clist"         %% "clist-core"          % "3.2.1",
      "org.backuity.clist"         %% "clist-macros"        % "3.2.1" % "provided",
      "com.typesafe.scala-logging" %% "scala-logging"       % "3.4.0",
      "ch.qos.logback"             %  "logback-classic"     % "1.1.7",
      "org.codehaus.groovy"        %  "groovy-all"          % "2.4.6",
      "org.scalatest"              %% "scalatest"           % "2.2.6" % Test
    )
}
