package org.monarchinitiative.dosdp.cli

import caseapp._
import org.apache.jena.sys.JenaSystem
import zio.{Config => _, _}
import zio.logging.slf4j.bridge.Slf4jBridge
import zio.logging.{ConsoleLoggerConfig, LogFilter, LogFormat, consoleLogger}

object Main extends ZCommandApp[Config] {

  override def appName: String = "dosdp-tools"

  override def progName: String = "dosdp-tools"

  /** Apply the given key/value pairs as structured log annotations for the span of `zio`. */
  def withLogContext[R, E, A](context: Map[String, String])(zio: ZIO[R, E, A]): ZIO[R, E, A] =
    ZIO.logAnnotate(context.map { case (k, v) => LogAnnotation(k, v) }.toSet)(zio)

  private def loggingLayer(verbose: Boolean): ZLayer[Any, Nothing, Unit] = {
    val level = if (verbose) LogLevel.Info else LogLevel.Warning
    val format = LogFormat.label("context", LogFormat.allAnnotations) |-| LogFormat.colored
    val config = ConsoleLoggerConfig(format, LogFilter.LogLevelByNameConfig(level))
    Runtime.removeDefaultLoggers >>> (consoleLogger(config) ++ Slf4jBridge.initialize)
  }

  override def run(config: Config, args: RemainingArgs): ZIO[Any, Nothing, ExitCode] = {
    val program = ZIO.succeed(JenaSystem.init()) *> config.run
    program
      .tapError { e =>
        if (config.common.verbose) ZIO.succeed(e.printStackTrace())
        else ZIO.logError(e.getMessage)
      }
      .as(ExitCode.success)
      // ZIOAppDefault derives the process exit code from success/failure of the
      // effect, not from a returned ExitCode value, so a non-zero exit must be
      // requested explicitly. exit avoids the framework's default cause dump,
      // leaving error reporting to the tapError above.
      .catchAll(_ => exit(ExitCode.failure).as(ExitCode.failure))
      .provideLayer(loggingLayer(config.common.verbose))
  }

}
