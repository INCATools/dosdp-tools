package org.monarchinitiative.dosdp.cli

import java.io.File
import java.nio.charset.StandardCharsets

import scala.collection.JavaConverters._

import org.backuity.clist._
import org.monarchinitiative.dosdp._
import org.monarchinitiative.dosdp.ExpandedDOSDP

import com.github.tototoshi.csv.CSVReader

import better.files._

object Terms extends Command(description = "dump terms referenced in TSV input and a Dead Simple OWL Design Pattern") with Common {

  var infile = opt[File](name = "infile", default = new File("fillers.tsv"), description = "Input file (TSV or CSV)")

  def run: Unit = {
    val sepFormat = tabularFormat
    val dosdp = inputDOSDP
    val eDOSDP = ExpandedDOSDP(dosdp, prefixes)
    val patternAxioms = eDOSDP.filledLogicalAxioms(None, None)
    val patternTerms = patternAxioms.flatMap(_.getSignature.asScala.map(_.getIRI).filterNot(_.toString.startsWith("urn:dosdp:")))
    val identifiers = (for {
      row <- CSVReader.open(infile, "utf-8")(sepFormat).iteratorWithHeaders
    } yield {
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
      varFillers ++ listVarFillers :+ iriBinding
    }).flatten.toSet
    val iris = patternTerms ++ identifiers.flatMap(Prefixes.idToIRI(_, prefixes))
    outfile.toScala.overwrite("").appendLines(iris.map(_.toString).toSeq: _*)(StandardCharsets.UTF_8)
  }

}