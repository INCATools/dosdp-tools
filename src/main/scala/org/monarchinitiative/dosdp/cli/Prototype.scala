package org.monarchinitiative.dosdp.cli

import java.io.File

import scala.collection.JavaConverters._

import org.backuity.clist._
import org.monarchinitiative.dosdp._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLAxiom

object Prototype extends Command(description = "output \"prototype\" axioms using default fillers for a pattern") with Common {

  def run: Unit = {
    val dosdp = inputDOSDP
    val fillers = dosdp.vars.getOrElse(Map.empty) ++
      dosdp.list_vars.getOrElse(Map.empty) ++
      dosdp.data_vars.getOrElse(Map.empty) ++
      dosdp.data_list_vars.getOrElse(Map.empty) +
      (DOSDP.DefinedClassVariable -> dosdp.pattern_iri.getOrElse(IRI.create(new File(templateFile)).toString)) //FIXME this is temporary until pattern_iri is standard; won't work with pattern specified by URI
    val axioms: Set[OWLAxiom] = Generate.renderPattern(dosdp, prefixes, fillers, ontologyOpt, true, true)
    val manager = OWLManager.createOWLOntologyManager()
    val ont = manager.createOntology(axioms.asJava)
    manager.saveOntology(ont, new FunctionalSyntaxDocumentFormat(), IRI.create(outfile))
  }

}