package org.monarchinitiative.dosdp.cli

import caseapp._
import org.apache.jena.sys.JenaSystem
import scribe.Level
import zio._

object Main extends ZCommandApp[Config] {

  override def run(config: Config, args: RemainingArgs): ZIO[ZEnv, Nothing, ExitCode] =
    ZIO.effectTotal(JenaSystem.init()) *>
      ZIO.effectTotal(scribe.Logger.root.clearHandlers().clearModifiers().withHandler(minimumLevel = Some(Level.Info)).replace()) *>
      config.run.exitCode

}
