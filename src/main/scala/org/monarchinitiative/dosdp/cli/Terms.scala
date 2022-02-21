package org.monarchinitiative.dosdp.cli

import better.files._
import com.github.tototoshi.csv.CSVReader
import org.monarchinitiative.dosdp.cli.DOSDPError.logError
import org.monarchinitiative.dosdp.cli.Main.loggingContext
import org.monarchinitiative.dosdp.{DOSDP, ExpandedDOSDP, Prefixes}
import zio._
import zio.logging._

import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._

object Terms {

  def run(config: TermsConfig): ZIO[ZEnv with Logging, DOSDPError, Unit] =
    log.locally(_.annotate(loggingContext, Map("command" -> "terms", "pattern" -> config.common.template, "input" -> config.infile, "output" -> config.common.outfile))) {
      for {
        dosdp <- config.common.inputDOSDP
        prefixes <- config.common.prefixesMap
        eDOSDP = ExpandedDOSDP(dosdp, prefixes)
        sepFormat <- Config.tabularFormat(config.common.tableFormat)
        patternAxioms <- eDOSDP.filledLogicalAxioms(None, None)
        patternTerms = patternAxioms.flatMap(_.getSignature.asScala.map(_.getIRI).filterNot(_.toString.startsWith("urn:dosdp:")))
        rows <- ZIO.effect(CSVReader.open(config.infile, StandardCharsets.UTF_8.name())(sepFormat)).bracketAuto(csvReader => ZIO.effect(csvReader.iteratorWithHeaders.toList))
          .flatMapError(e => logError(s"Could not read fillers file at ${config.infile}", e))
        identifiers = rows.flatMap(identifiersForRow(_, dosdp)).to(Set)
        iris = patternTerms ++ identifiers.flatMap(Prefixes.idToIRI(_, prefixes)) //FIXME should we report failure to expand to IRI?
        _ <- ZIO.effect(config.common.outfile.toFile.overwrite("").appendLines(iris.map(_.toString).toSeq: _*)(StandardCharsets.UTF_8))
          .flatMapError(e => logError(s"Failed writing output file at ${config.common.outfile}", e))
      } yield ()
    }

  private def identifiersForRow(row: Map[String, String], dosdp: DOSDP): Set[String] = {
    val varFillers = for {
      vars <- dosdp.vars.toSeq
      varr <- vars.keys
      filler <- row.get(varr)
    } yield filler.trim
    val listVarFillers = for {
      listVars <- dosdp.list_vars.toSeq
      listVar <- listVars.keys
      filler <- row.get(listVar).toSeq
      item <- filler.split(DOSDP.MultiValueDelimiter)
    } yield item.trim
    val iriBinding = row(DOSDP.DefinedClassVariable).trim
    (varFillers ++ listVarFillers :+ iriBinding).to(Set)
  }

}

