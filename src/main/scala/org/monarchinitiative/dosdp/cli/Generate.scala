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

import cats.implicits._
import org.semanticweb.owlapi.model.OWLAxiom

object Generate extends Command(description = "generate ontology axioms for TSV input to a Dead Simple OWL Design Pattern") with Common {

  var infile = opt[File](name = "infile", default = new File("fillers.tsv"), description = "Input file (TSV or CSV)")
  var restrictAxioms = opt[String](name = "restrict-axioms-to", default = "all", description = "Restrict generated axioms to 'logical', 'annotation', or 'all' (default)")
  var restrictAxiomsColumn = opt[Option[String]](name = "restrict-axioms-column", description = "Data column containing local axiom output restrictions")

  val LocalLabelProperty = IRI.create("http://example.org/TSVProvidedLabel")

  def run: Unit = {
    val (outputLogicalAxioms, outputAnnotationAxioms) = restrictAxioms match {
      case "all"        => (true, true)
      case "logical"    => (true, false)
      case "annotation" => (false, true)
      case other        => throw new UnsupportedOperationException(s"Invalid argument for restrict-axioms-to: $other")
    }
    val sepFormat = tabularFormat
    val dosdp = inputDOSDP
    val axioms: Set[OWLAxiom] = renderPattern(dosdp, prefixes, CSVReader.open(infile, "utf-8")(sepFormat).iteratorWithHeaders, ontologyOpt, outputLogicalAxioms, outputAnnotationAxioms, restrictAxiomsColumn)
    val manager = OWLManager.createOWLOntologyManager()
    val ont = manager.createOntology(axioms.asJava)
    manager.saveOntology(ont, new FunctionalSyntaxDocumentFormat(), IRI.create(outfile))
  }

  def renderPattern(dosdp: DOSDP, prefixes: PartialFunction[String, String], fillers: Map[String, String], ontOpt: Option[OWLOntology], outputLogicalAxioms: Boolean, outputAnnotationAxioms: Boolean, restrictAxiomsColumnName: Option[String]): Set[OWLAxiom] =
    renderPattern(dosdp, prefixes, Seq(fillers).iterator, ontOpt, outputLogicalAxioms, outputAnnotationAxioms, restrictAxiomsColumnName)

  def renderPattern(dosdp: DOSDP, prefixes: PartialFunction[String, String], fillers: Iterator[Map[String, String]], ontOpt: Option[OWLOntology], outputLogicalAxioms: Boolean, outputAnnotationAxioms: Boolean, restrictAxiomsColumnName: Option[String]): Set[OWLAxiom] = {
    val eDOSDP = ExpandedDOSDP(dosdp, prefixes)
    val readableIDIndex = ontOpt.map(ont => createReadableIdentifierIndex(eDOSDP, ont)).getOrElse(Map.empty)
    (for {
      row <- fillers
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
      val readableIDIndexPlusLocalLabels = readableIDIndex + localLabels
      val initialAnnotationBindings = varBindings.mapValues(v => irisToLabels(v, eDOSDP, readableIDIndexPlusLocalLabels)) ++
        listVarBindings.mapValues(v => irisToLabels(v, eDOSDP, readableIDIndexPlusLocalLabels)) ++
        dataVarBindings ++
        dataListBindings +
        iriBinding
      val annotationBindings = eDOSDP.substitutions.foldLeft(initialAnnotationBindings)((bindings, sub) => sub.expandBindings(bindings))
      val (localOutputLogicalAxioms, localOutputAnnotationAxioms) = restrictAxiomsColumnName.flatMap(column => row.get(column)).map(_.trim).map {
        case "all"        => (true, true)
        case "logical"    => (true, false)
        case "annotation" => (false, true)
        case ""           => (outputLogicalAxioms, outputAnnotationAxioms)
        case other        => throw new UnsupportedOperationException(s"Invalid value for restrict-axioms-column: $other")
      }.getOrElse((outputLogicalAxioms, outputAnnotationAxioms))
      val logicalAxioms = if (localOutputLogicalAxioms) eDOSDP.filledLogicalAxioms(Some(logicalBindings), Some(annotationBindings)) else Set.empty
      val annotationAxioms = if (localOutputAnnotationAxioms) eDOSDP.filledAnnotationAxioms(Some(annotationBindings)) else Set.empty
      logicalAxioms ++ annotationAxioms
    }).toSet.flatten
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
      case prop if index.get(prop).exists(_.isDefinedAt(iri)) => index(prop)(iri)
    }
    labelOpt.getOrElse(iri.toString)
  }

}