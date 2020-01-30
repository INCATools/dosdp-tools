package org.monarchinitiative.dosdp.cli

import java.io.{File, FileReader}

import com.github.tototoshi.csv.{CSVFormat, DefaultCSVFormat, TSVFormat}
import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.yaml.parser
import org.backuity.clist._
import org.monarchinitiative.dosdp._
import org.obolibrary.robot.CatalogXmlIRIMapper
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI, OWLOntology}

import scala.io.Source

trait Common extends Command with LazyLogging {

  def run(): Unit

  var ontOpt = opt[Option[String]](name = "ontology", description = "OWL ontology (provide labels, query axioms)")
  var catalogFileOpt = opt[Option[File]](name = "catalog", description = "catalog file to use for resolving ontology locations")
  var templateFile = opt[String](name = "template", default = "dosdp.yaml", description = "DOSDP file (YAML). If a local file is not found at the given path, the path will be attempted as a URL.")
  var prefixesFileOpt = opt[Option[File]](name = "prefixes", default = None, description = "CURIE prefixes (YAML)")
  var oboPrefixes = opt[Boolean](name = "obo-prefixes", default = false, description = "Assume prefixes are OBO ontologies; predefine rdf, rdfs, owl, dc, dct, skos, obo, and oio.")
  var outfile = opt[File](name = "outfile", default = new File("dosdp.out"), description = "Output file (OWL or TSV)")
  var tableFormat = opt[String](name = "table-format", default = "tsv", description = "Tabular format: TSV (default) or CSV")
  var batchPatterns = opt[Seq[String]](name = "batch-patterns", description = "List of patterns (without file extension) to process in batch (space separated, enclose list in quotes)", default = Nil)

  def ontologyOpt: Option[OWLOntology] = ontOpt.map { ontPath =>
    val ontIRI = if (ontPath.startsWith("http")) IRI.create(ontPath) else IRI.create(new File(ontPath))
    val manager = OWLManager.createOWLOntologyManager()
    catalogFileOpt.foreach(catalog => manager.addIRIMapper(new CatalogXmlIRIMapper(catalog)))
    manager.loadOntology(ontIRI)
  }

  def inputDOSDP: DOSDP = inputDOSDPFrom(templateFile)

  def inputDOSDPFrom(location: String): DOSDP = {
    val possibleFile = new File(location)
    val source = if (possibleFile.exists) Source.fromFile(possibleFile, "UTF-8")
    else Source.fromURL(location, "UTF-8")
    parser.parse(source.mkString).right.flatMap(json => json.as[DOSDP]) match {
      case Right(dosdp) => dosdp
      case Left(error)  =>
        logger.error(s"Failed to parse pattern:\n${error.getMessage}")
        throw error
    }
  }

  def prefixes: PartialFunction[String, String] = {
    val specifiedPrefixes = (for {
      prefixesFile <- prefixesFileOpt
      prefixesJson <- parser.parse(new FileReader(prefixesFile)).right.toOption
      prefixMap <- prefixesJson.as[Map[String, String]].right.toOption
    } yield prefixMap).getOrElse(Map.empty)
    if (oboPrefixes) specifiedPrefixes.orElse(OBOPrefixes) else specifiedPrefixes
  }

  def tabularFormat: CSVFormat = tableFormat.toLowerCase match {
    case "csv" => new DefaultCSVFormat {}
    case "tsv" => new TSVFormat {}
    case other => throw new RuntimeException(s"Invalid tabular format requested: $other")
  }

}