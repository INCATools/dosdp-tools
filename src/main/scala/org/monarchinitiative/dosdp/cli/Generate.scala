package org.monarchinitiative.dosdp.cli

import java.io.File
import java.io.FileReader

import org.backuity.clist._
import org.phenoscape.owlet.Owlet
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.yaml.parser
import org.apache.jena.riot.RDFDataMgr

import java.io.FileOutputStream
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.query.ResultSetFormatter

import scala.collection.JavaConverters._
import org.semanticweb.owlapi.model.OWLOntology
import org.apache.jena.rdf.model.ModelFactory
import org.monarchinitiative.dosdp._
import org.phenoscape.scowl._

import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.TSVFormat
import org.semanticweb.owlapi.apibinding.OWLManager
import scala.collection.JavaConverters._
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.monarchinitiative.dosdp.Binding
import org.monarchinitiative.dosdp.ExpandedDOSDP
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.parameters.Imports
import cats.implicits._

object Generate extends Command(description = "generate ontology axioms for TSV input to a Dead Simple OWL Design Pattern") with Common {

  var infile = opt[File](name = "infile", default = new File("fillers.tsv"), description = "Input file (TSV)")

  def run: Unit = {
    for {
      dosdp <- inputDOSDP.right
    } {
      val eDOSDP = ExpandedDOSDP(dosdp, prefixes)
      val readableIDIndex = ontologyOpt.map(ont => createReadableIdentifierIndex(eDOSDP, ont)).getOrElse(Map.empty)
      val axioms = (for {
        row <- CSVReader.open(infile, "utf-8")(new TSVFormat {}).iteratorWithHeaders
      } yield {
        val varBindings = (for {
          vars <- dosdp.vars.toSeq
          varr <- vars.keys
          filler <- row.get(varr)
        } yield varr -> SingleValue(filler.trim)).toMap
        val listVarBindings = (for {
          listVars <- dosdp.list_vars.toSeq
          listVar <- listVars.keys
          filler <- row.get(listVar)
        } yield listVar -> MultiValue(filler.split(DOSDP.MultiValueDelimiter).map(_.trim).toSet)).toMap
        val dataVarBindings = (for {
          dataVars <- dosdp.data_vars.toSeq
          dataVar <- dataVars.keys
          filler <- row.get(dataVar)
        } yield dataVar -> SingleValue(filler.trim)).toMap
        val dataListBindings = (for {
          dataListVars <- dosdp.data_list_vars.toSeq
          dataListVar <- dataListVars.keys
          filler <- row.get(dataListVar)
        } yield dataListVar -> MultiValue(filler.split(DOSDP.MultiValueDelimiter).map(_.trim).toSet)).toMap
        val iriBinding = DOSDP.DefinedClassVariable -> SingleValue(row(DOSDP.DefinedClassVariable).trim)
        val logicalBindings = varBindings + iriBinding
        val annotationBindings = varBindings.mapValues(v => irisToLabels(v, eDOSDP, readableIDIndex)) ++
          listVarBindings.mapValues(v => irisToLabels(v, eDOSDP, readableIDIndex)) ++
          dataVarBindings ++
          dataListBindings +
          iriBinding
        eDOSDP.filledLogicalAxioms(Some(logicalBindings)) ++ eDOSDP.filledAnnotationAxioms(Some(annotationBindings))
      }).toSet.flatten

      val manager = OWLManager.createOWLOntologyManager()
      val ont = manager.createOntology(axioms.asJava)
      manager.saveOntology(ont, new FunctionalSyntaxDocumentFormat(), IRI.create(outfile))
    }
  }

  private def createReadableIdentifierIndex(dosdp: ExpandedDOSDP, ont: OWLOntology): Map[IRI, Map[IRI, String]] = {
    val properties = dosdp.readableIdentifierProperties.toSet
    val mappings = for {
      AnnotationAssertion(_, prop, subj: IRI, value ^^ _) <- ont.getAxioms(AxiomType.ANNOTATION_ASSERTION, Imports.INCLUDED).asScala
      if properties(prop)
    } yield Map(prop.getIRI -> Map(subj -> value))
    mappings.fold(Map.empty)(_ combine _)
  }

  private def irisToLabels(binding: Binding, dosdp: ExpandedDOSDP, index: Map[IRI, Map[IRI, String]]): Binding = binding match {
    case SingleValue(value) => SingleValue(dosdp.checker.idToIRI(value).map(iri => readableIdentifierForIRI(iri, dosdp, index)).getOrElse(value))
    case MultiValue(values) => MultiValue(values.map(value => dosdp.checker.idToIRI(value).map(iri => readableIdentifierForIRI(iri, dosdp, index)).getOrElse(value)))
  }

  private def readableIdentifierForIRI(iri: IRI, dosdp: ExpandedDOSDP, index: Map[IRI, Map[IRI, String]]): String = {
    val labelOpt = dosdp.readableIdentifierProperties.collectFirst {
      case prop if index.get(prop.getIRI).map(_.isDefinedAt(iri)).getOrElse(false) => index(prop.getIRI)(iri)
    }
    labelOpt.getOrElse(iri.toString)
  }

}