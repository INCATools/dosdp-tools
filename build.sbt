enablePlugins(JavaAppPackaging)
enablePlugins(BuildInfoPlugin)
enablePlugins(GitVersioning)
enablePlugins(LauncherJarPlugin)

organization  := "org.monarchinitiative"

name          := "dosdp-tools"

version       := "0.20.0"

scalaVersion  := "2.13.18"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

Compile / mainClass := Some("org.monarchinitiative.dosdp.cli.Main")

javaOptions += "-Xmx8G"

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

val gitCommitString = SettingKey[String]("gitCommit")

gitCommitString := git.gitHeadCommit.value.getOrElse("Not Set")

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, gitCommitString)

buildInfoPackage := "org.monarchinitiative.dosdp.cli"

val zioVersion = "2.1.26"
val zioLoggingVersion = "2.5.3"
val catsVersion = "2.13.0"
val circeVersion = "0.14.15"
val circeYamlVersion = "0.16.1"

libraryDependencies ++= {
    Seq(
      "dev.zio"                    %% "zio"                    % zioVersion,
      "com.github.alexarchambault" %% "case-app"               % "2.0.6",
      "net.sourceforge.owlapi"     %  "owlapi-distribution"    % "4.5.29",
      "org.phenoscape"             %% "scowl"                  % "1.4.1",
      "io.github.liveontologies"   %  "elk-owlapi"             % "0.6.0"
        exclude("net.sourceforge.owlapi", "owlapi-apibinding")
        exclude("net.sourceforge.owlapi", "owlapi-api")
        exclude("net.sourceforge.owlapi", "owlapi-impl"),
      "net.sourceforge.owlapi"     %  "org.semanticweb.hermit" % "1.4.3.456",
      "net.sourceforge.owlapi"     %  "jfact"                  % "4.0.4",
      "org.typelevel"              %% "cats-core"              % catsVersion,
      "io.circe"                   %% "circe-yaml"             % circeYamlVersion,
      "io.circe"                   %% "circe-core"             % circeVersion,
      "io.circe"                   %% "circe-generic"          % circeVersion,
      "com.github.pathikrit"       %% "better-files"           % "3.9.2",
      "org.apache.jena"            %  "jena-arq"               % "4.10.0",
      "org.apache.jena"            %  "jena-core"              % "4.10.0",
      "com.github.tototoshi"       %% "scala-csv"              % "2.0.0",
      "commons-codec"              %  "commons-codec"          % "1.17.2",
      "dev.zio"                    %% "zio-logging"            % zioLoggingVersion,
      "dev.zio"                    %% "zio-logging-slf4j2-bridge" % zioLoggingVersion,
      "dev.zio"                    %% "zio-test"               % zioVersion % Test,
      "dev.zio"                    %% "zio-test-sbt"           % zioVersion % Test
    )
}
