enablePlugins(JavaAppPackaging)

organization  := "org.monarchinitiative"

name          := "dosdp-tools"

version       := "0.17"

scalaVersion  := "2.13.7"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

Compile / mainClass := Some("org.monarchinitiative.dosdp.cli.Main")

javaOptions += "-Xmx8G"

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

val zioVersion = "1.0.12"

libraryDependencies ++= {
    Seq(
      "dev.zio"                    %% "zio"                    % zioVersion,
      "dev.zio"                    %% "zio-streams"            % zioVersion,
      "com.github.alexarchambault" %% "case-app"               % "2.0.6",
      "net.sourceforge.owlapi"     %  "owlapi-distribution"    % "4.5.20",
      "org.phenoscape"             %% "scowl"                  % "1.4.0",
      "org.phenoscape"             %% "owlet"                  % "1.9" exclude("org.slf4j", "slf4j-log4j12"),
      "org.semanticweb.elk"        %  "elk-owlapi"             % "0.4.3" exclude("org.slf4j", "slf4j-log4j12"),
      "net.sourceforge.owlapi"     %  "org.semanticweb.hermit" % "1.4.3.456",
      "net.sourceforge.owlapi"     %  "jfact"                  % "4.0.4",
      "org.geneontology"           %% "owl-diff"               % "1.2.2",
      "io.circe"                   %% "circe-yaml"             % "0.14.0",
      "io.circe"                   %% "circe-core"             % "0.13.0",
      "io.circe"                   %% "circe-generic"          % "0.13.0",
      "io.circe"                   %% "circe-parser"           % "0.13.0",
      "org.obolibrary.robot"       %  "robot-core"             % "1.8.1"
        exclude("org.slf4j", "slf4j-log4j12")
        exclude("org.geneontology", "whelk_2.12")
        exclude("org.geneontology", "whelk-owlapi_2.12")
        exclude("org.geneontology", "owl-diff_2.12"),
      "com.github.pathikrit"       %% "better-files"           % "3.9.1",
      "org.apache.jena"            %  "apache-jena-libs"       % "4.2.0" exclude("org.slf4j", "slf4j-log4j12"),
      "com.github.tototoshi"       %% "scala-csv"              % "1.3.8",
      "commons-codec"              %  "commons-codec"          % "1.15",
      "com.outr"                   %% "scribe-slf4j"           % "3.6.3",
      "dev.zio"                    %% "zio-test"               % zioVersion % Test,
      "dev.zio"                    %% "zio-test-sbt"           % zioVersion % Test
    )
}
