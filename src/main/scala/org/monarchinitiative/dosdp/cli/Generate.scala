package org.monarchinitiative.dosdp.cli

import java.io.File

import scala.collection.JavaConverters._

import org.backuity.clist._
import org.monarchinitiative.dosdp._
import org.monarchinitiative.dosdp.Binding
import org.monarchinitiative.dosdp.ExpandedDOSDP
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.parameters.Imports

import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.TSVFormat

import cats.implicits._

object Generate extends Command(description = "generate ontology axioms for TSV input to a Dead Simple OWL Design Pattern") with Common {

  var infile = opt[File](name = "infile", default = new File("fillers.tsv"), description = "Input file (TSV)")

  val LocalLabelProperty = IRI.create("http://example.org/TSVProvidedLabel")

  def run: Unit = {
    val dosdp = inputDOSDP
    val eDOSDP = ExpandedDOSDP(dosdp, prefixes)
    val readableIDIndex = ontologyOpt.map(ont => createReadableIdentifierIndex(eDOSDP, ont)).getOrElse(Map.empty)
    val axioms = (for {
      row <- CSVReader.open(infile, "utf-8")(new TSVFormat {}).iteratorWithHeaders
    } yield {
      val (varBindingsItems, localLabelItems) = (for {
        vars <- dosdp.vars.toSeq
        varr <- vars.keys
        filler <- row.get(varr)
        fillerLabelOpt = for {
          fillerIRI <- Prefixes.idToIRI(filler, prefixes)
          label <- row.get(s"${varr}_label")
        } yield fillerIRI -> label
      } yield (varr -> SingleValue(filler.trim), fillerLabelOpt)).unzip
      val varBindings = varBindingsItems.toMap
      val localLabels = LocalLabelProperty -> localLabelItems.flatten.toMap
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
      val readableIDIndexPlusTSV = readableIDIndex + localLabels
      val annotationBindings = varBindings.mapValues(v => irisToLabels(v, eDOSDP, readableIDIndexPlusTSV)) ++
        listVarBindings.mapValues(v => irisToLabels(v, eDOSDP, readableIDIndexPlusTSV)) ++
        dataVarBindings ++
        dataListBindings +
        iriBinding
      eDOSDP.filledLogicalAxioms(Some(logicalBindings)) ++ eDOSDP.filledAnnotationAxioms(Some(annotationBindings))
    }).toSet.flatten

    val manager = OWLManager.createOWLOntologyManager()
    val ont = manager.createOntology(axioms.asJava)
    manager.saveOntology(ont, new FunctionalSyntaxDocumentFormat(), IRI.create(outfile))
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
    case SingleValue(value) => SingleValue(Prefixes.idToIRI(value, dosdp.prefixes).map(iri => readableIdentifierForIRI(iri, dosdp, index)).getOrElse(value))
    case MultiValue(values) => MultiValue(values.map(value => Prefixes.idToIRI(value, dosdp.prefixes).map(iri => readableIdentifierForIRI(iri, dosdp, index)).getOrElse(value)))
  }

  private def readableIdentifierForIRI(iri: IRI, dosdp: ExpandedDOSDP, index: Map[IRI, Map[IRI, String]]): String = {
    val properties = LocalLabelProperty :: dosdp.readableIdentifierProperties.map(_.getIRI)
    val labelOpt = properties.collectFirst {
      case prop if index.get(prop).map(_.isDefinedAt(iri)).getOrElse(false) => index(prop)(iri)
    }
    labelOpt.getOrElse(iri.toString)
  }

}