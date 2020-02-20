package org.monarchinitiative.dosdp.cli

import org.apache.jena.sys.JenaSystem
import org.backuity.clist._
import scribe.Level

object Main extends App {

  JenaSystem.init()
  try {
    scribe.Logger.root.clearHandlers().clearModifiers().withHandler(minimumLevel = Some(Level.Info)).replace()
    Cli.parse(args).withProgramName("dosdp-tools").withCommands(Generate, Query, Terms, Prototype).foreach(_.run())
  } catch {
    case e: Exception =>
      println(e.getMessage)
      throw e
  }

}
