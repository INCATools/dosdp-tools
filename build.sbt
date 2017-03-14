enablePlugins(JavaAppPackaging)

organization  := "org.monarchinitiative"

name          := "dosdp-scala"

version       := "0.1"

scalaVersion  := "2.11.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

mainClass in Compile := Some("org.monarchinitiative.dosdp.Main")

resolvers += Resolver.bintrayRepo("jeremyrsmith", "maven")

resolvers += "Phenoscape Maven repository" at "http://phenoscape.svn.sourceforge.net/svnroot/phenoscape/trunk/maven/repository"

javaOptions += "-Xmx8G"

libraryDependencies ++= {
    Seq(
      "net.sourceforge.owlapi"     %  "owlapi-distribution" % "4.2.1",
      "org.phenoscape"             %% "scowl"               % "1.1",
      "org.phenoscape"             %% "owlet"               % "1.4" exclude("org.slf4j", "slf4j-log4j12"),
      "org.semanticweb.elk"        %  "elk-owlapi"          % "0.4.3",
      "io.circe"                   %% "circe-core"          % "0.5.0-M2",
      "io.circe"                   %% "circe-generic"       % "0.5.0-M2",
      "io.circe"                   %% "circe-parser"        % "0.5.0-M2",
      "io.github.jeremyrsmith"     %% "circe-yaml"          % "0.2.1",
      "org.apache.jena"            %  "apache-jena-libs"    % "2.10.1" exclude("org.slf4j", "slf4j-log4j12"),
      "org.backuity.clist"         %% "clist-core"          % "3.2.1",
      "org.backuity.clist"         %% "clist-macros"        % "3.2.1" % "provided",
      "com.typesafe.scala-logging" %% "scala-logging"       % "3.4.0",
      "ch.qos.logback"             %  "logback-classic"     % "1.1.7",
      "org.codehaus.groovy"        %  "groovy-all"          % "2.4.6",
      "org.scalatest"              %% "scalatest"           % "2.2.6" % Test
    )
}
