enablePlugins(JavaAppPackaging)

organization  := "org.monarchinitiative"

name          := "dosdp-tools"

version       := "0.11"

scalaVersion  := "2.12.7"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

mainClass in Compile := Some("org.monarchinitiative.dosdp.cli.Main")

javaOptions += "-Xmx8G"

scriptClasspath := Seq("*")

libraryDependencies ++= {
    Seq(
      "net.sourceforge.owlapi"     %  "owlapi-distribution"    % "5.1.9",
      "org.phenoscape"             %% "scowl"                  % "1.3.1",
      "org.phenoscape"             %% "owlet"                  % "1.6.1" exclude("org.slf4j", "slf4j-log4j12"),
      "org.semanticweb.elk"        %  "elk-owlapi"             % "0.4.3" exclude("org.slf4j", "slf4j-log4j12"),
      "net.sourceforge.owlapi"     %  "org.semanticweb.hermit" % "1.4.0.432",
      "net.sourceforge.owlapi"     %  "jfact"                  % "4.0.4",
      "org.obolibrary.robot"       %  "robot-core"             % "1.1.0" exclude("org.slf4j", "slf4j-log4j12"),
      "io.circe"                   %% "circe-core"             % "0.10.1",
      "io.circe"                   %% "circe-generic"          % "0.10.1",
      "io.circe"                   %% "circe-parser"           % "0.10.1",
      "io.circe"                   %% "circe-yaml"             % "0.9.0",
      "com.github.pathikrit"       %% "better-files"           % "3.6.0",
      "org.apache.jena"            %  "apache-jena-libs"       % "3.9.0" exclude("org.slf4j", "slf4j-log4j12"),
      "org.backuity.clist"         %% "clist-core"             % "3.5.0",
      "org.backuity.clist"         %% "clist-macros"           % "3.5.0" % "provided",
      "com.github.tototoshi"       %% "scala-csv"              % "1.3.5",
      "commons-codec"              %  "commons-codec"          % "1.11",
      "com.typesafe.scala-logging" %% "scala-logging"          % "3.9.0",
      "ch.qos.logback"             %  "logback-classic"        % "1.2.3",
      "org.codehaus.groovy"        %  "groovy-all"             % "2.5.4",
      "org.scalatest"              %% "scalatest"              % "3.0.5" % Test
    )
}
