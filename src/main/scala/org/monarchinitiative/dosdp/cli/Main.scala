package org.monarchinitiative.dosdp.cli

import caseapp._
import org.apache.jena.sys.JenaSystem
import org.monarchinitiative.dosdp.DOSDP
import scribe._
import scribe.filter._
import zio._

import java.time.{Duration, Instant}

object Main extends ZCommandApp[Config] {

  override def appName: String = "dosdp-tools"

  override def progName: String = "dosdp-tools"

  def toHumanReadableDateDiff(start: Long, end: Long): String = {
    val seconds = Duration.between(Instant.ofEpochMilli(start), Instant.ofEpochMilli(end)).getSeconds
    String.format("%d:%02d:%02d", seconds / 3600, (seconds % 3600) / 60, seconds % 60);
  }

  override def run(config: Config, args: RemainingArgs): ZIO[ZEnv, Nothing, ExitCode] = {
    val program = ZIO.effectTotal(JenaSystem.init()) *> ZIO.effectTotal(
      scribe.Logger.root
        .clearHandlers()
        .clearModifiers()
        .withModifier(select(packageName(DOSDP.getClass.getPackage.getName)).include(level >= Level.Info))
        .withHandler(minimumLevel = Some(if (config.common.verbose) Level.Info else Level.Warn))
        .replace()
    ) *> config.run
    program
      .as(ExitCode.success)
      .catchAll { case DOSDPError(msg, e) =>
        if (config.common.verbose) ZIO.effectTotal(e.printStackTrace()).as(ExitCode.failure)
        else ZIO.effectTotal(scribe.error(msg)).as(ExitCode.failure)
      }
  }

}
