package org.monarchinitiative.dosdp.cli

import caseapp._
import org.apache.jena.sys.JenaSystem
import org.monarchinitiative.dosdp.DOSDP
import scribe._
import scribe.filter._
import zio._
import zio.console.putStrLn

import java.time.{Duration, Instant}
import java.util.concurrent.TimeUnit

object Main extends ZCommandApp[Config] {

  override def appName: String = "dosdp-tools"

  override def progName: String = "dosdp-tools"

  def toHumanReadableDateDiff(start: Long, end: Long): String = {
    val seconds = Duration.between(Instant.ofEpochMilli(start), Instant.ofEpochMilli(end)).getSeconds
    String.format("%d:%02d:%02d", seconds / 3600, (seconds % 3600) / 60, seconds % 60);
  }

  override def run(config: Config, args: RemainingArgs): ZIO[ZEnv, Nothing, ExitCode] = {
    val program = for {
      start <- clock.currentTime(TimeUnit.MILLISECONDS)
      _ <- ZIO.effectTotal(JenaSystem.init()) *> ZIO.effectTotal(
        scribe.Logger.root
          .clearHandlers()
          .clearModifiers()
          .withModifier(select(packageName(DOSDP.getClass.getPackage.getName)).include(level >= Level.Info))
          .withHandler(minimumLevel = Some(Level.Warn))
          .replace()
      )
      exitCode <- config.run.exitCode
      end <- clock.currentTime(TimeUnit.MILLISECONDS)
      duration = toHumanReadableDateDiff(start, end)
      _ <- ZIO.effect(scribe.info(s"Duration: $duration"))
    } yield exitCode
    program
      .catchSome { case DOSDPError(msg, e) =>
        ZIO.effectTotal(scribe.error(msg)).as(ExitCode.failure)
      }
      .catchAllCause(cause => putStrLn(cause.untraced.prettyPrint).as(ExitCode.failure))
  }

}
