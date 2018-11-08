package org.monarchinitiative.dosdp.cli

import org.apache.jena.sys.JenaSystem
import org.backuity.clist._

object Main extends App {

  JenaSystem.init()
  try {
    Cli.parse(args).withProgramName("dosdp-tools").withCommands(Generate, Query, Terms, Prototype).foreach(_.run())
  } catch {
    case e: Exception =>
      println(e.getMessage)
      throw e
  }

}
