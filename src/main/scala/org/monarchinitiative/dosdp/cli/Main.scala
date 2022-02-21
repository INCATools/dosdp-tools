package org.monarchinitiative.dosdp.cli

import caseapp._
import org.apache.jena.sys.JenaSystem
import zio._
import zio.logging._
import zio.logging.slf4j.bridge.initializeSlf4jBridge

import java.time.{Duration, Instant}

object Main extends ZCommandApp[Config] {

  val loggingContext: LogAnnotation[Map[String, String]] = LogAnnotation(
    name = "context",
    initialValue = Map.empty,
    combine = _ ++ _,
    render = _.foldLeft("") { case (text, (key, value)) =>
      val separator = if (text.nonEmpty) ";" else ""
      s"$text$separator$key=$value"
    }
  )

  override def appName: String = "dosdp-tools"

  override def progName: String = "dosdp-tools"

  def toHumanReadableDateDiff(start: Long, end: Long): String = {
    val seconds = Duration.between(Instant.ofEpochMilli(start), Instant.ofEpochMilli(end)).getSeconds
    String.format("%d:%02d:%02d", seconds / 3600, (seconds % 3600) / 60, seconds % 60);
  }

  override def run(config: Config, args: RemainingArgs): ZIO[ZEnv, Nothing, ExitCode] = {
    val env =
      Logging.console(
        logLevel = if (config.common.verbose) LogLevel.Info else LogLevel.Warn,
        format = LogFormat.ColoredLogFormat { (context, line) =>
          val c = loggingContext.render(context.get(loggingContext))
          s"[context: $c] $line"
        }
      ) >>> initializeSlf4jBridge
    val program = ZIO.effectTotal(JenaSystem.init()) *> config.run
    program
      .as(ExitCode.success)
      .catchAll { case DOSDPError(_, e) =>
        if (config.common.verbose) ZIO.effectTotal(e.printStackTrace()).as(ExitCode.failure)
        else ZIO.unit.as(ExitCode.failure)
      }
      .provideCustomLayer(env)
  }

}
