package org.monarchinitiative.dosdp.cli

import better.files._
import File._
import scala.collection.JavaConverters._

import org.backuity.clist._
import org.monarchinitiative.dosdp._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom

object Prototype extends Command(description = "output \"prototype\" axioms using default fillers for a pattern or folder of patterns") with Common {

  def run: Unit = {
    val possibleFile = File(templateFile)
    val filenames = if (possibleFile.isDirectory) {
      possibleFile.list.filter { f =>
        println(f)
        f.extension(false, false, true).map { ext =>
          (ext == "yaml") || (ext == "yml")
        }.getOrElse(false)
      }.map(_.toString).toSet
    } else Set(templateFile)
    println(filenames)
    val dosdps = filenames.map(inputDOSDPFrom)
    val axioms = for {
      dosdp <- dosdps
      axiom <- axiomsFor(dosdp)
    } yield axiom
    val manager = OWLManager.createOWLOntologyManager()
    val ont = manager.createOntology(axioms.asJava)
    manager.saveOntology(ont, new FunctionalSyntaxDocumentFormat(), IRI.create(outfile))
  }

  private def axiomsFor(dosdp: DOSDP): Set[OWLAxiom] =
    dosdp.pattern_iri.map { iri =>
      val fillers = dosdp.vars.getOrElse(Map.empty) ++
        dosdp.list_vars.getOrElse(Map.empty) ++
        dosdp.data_vars.getOrElse(Map.empty) ++
        dosdp.data_list_vars.getOrElse(Map.empty) +
        (DOSDP.DefinedClassVariable -> iri)
      Generate.renderPattern(dosdp, prefixes, fillers, ontologyOpt, true, true)
    }.toSet.flatten

}