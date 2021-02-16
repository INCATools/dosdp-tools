package org.monarchinitiative.dosdp.cli

import caseapp._
import org.apache.jena.sys.JenaSystem
import org.monarchinitiative.dosdp.DOSDP
import scribe._
import scribe.filter._
import zio._

object Main extends ZCommandApp[Config] {

  override def appName: String = "dosdp-tools"

  override def progName: String = "dosdp-tools"

  override def run(config: Config, args: RemainingArgs): ZIO[ZEnv, Nothing, ExitCode] =
    ZIO.effectTotal(JenaSystem.init()) *>
      ZIO.effectTotal(
          scribe.Logger.root
            .clearHandlers()
            .clearModifiers()
            .withModifier(select(packageName(DOSDP.getClass.getPackageName)).include(level >= Level.Info))
            .withHandler(minimumLevel = Some(Level.Warn))
            .replace()
      ) *>
      config.run.exitCode

}
