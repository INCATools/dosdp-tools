package org.monarchinitiative.dosdp.cli

import org.backuity.clist._
import java.io.File
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import scala.util.Either
import io.circe.ParsingFailure
import io.circe.Error
import java.io.FileReader

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.yaml.parser
import org.monarchinitiative.dosdp._
import com.typesafe.scalalogging.LazyLogging

trait Common extends Command with LazyLogging {

  def run(): Unit

  var ontOpt = opt[Option[String]](name = "ontology", description = "OWL ontology (provide labels, query axioms)")
  var templateFile = opt[File](name = "template", default = new File("dosdp.yaml"), description = "DOSDP file (YAML)")
  var prefixesFileOpt = opt[Option[File]](name = "prefixes", default = None, description = "CURIE prefixes (YAML)")
  var oboPrefixes = opt[Boolean](name = "obo-prefixes", default = false, description = "Assume prefixes are OBO ontologies; predefine rdf, rdfs, and owl")
  var outfile = opt[File](name = "outfile", default = new File("dosdp.out"), description = "Output file (OWL or TSV)")

  def ontologyOpt: Option[OWLOntology] = ontOpt.map { ontPath =>
    val ontIRI = if (ontPath.startsWith("http")) IRI.create(ontPath) else IRI.create(new File(ontPath))
    val manager = OWLManager.createOWLOntologyManager()
    manager.loadOntology(ontIRI)
  }

  def inputDOSDP: DOSDP = parser.parse(new FileReader(templateFile)).right.flatMap(json => json.as[DOSDP]) match {
    case Right(dosdp) => dosdp
    case Left(error) =>
      logger.error(s"Failed to parse pattern:\n${error.getMessage}")
      throw error
  }

  def prefixes: PartialFunction[String, String] = {
    val specifiedPrefixes = (for {
      prefixesFile <- prefixesFileOpt
      prefixesJson <- parser.parse(new FileReader(prefixesFile)).right.toOption
      prefixMap <- prefixesJson.as[Map[String, String]].right.toOption
    } yield prefixMap).getOrElse(Map.empty)
    if (oboPrefixes) specifiedPrefixes.orElse(OBOPrefixes) else specifiedPrefixes
  }

}