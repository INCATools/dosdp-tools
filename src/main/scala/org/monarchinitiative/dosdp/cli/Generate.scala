package org.monarchinitiative.dosdp.cli

import java.io.{File, IOException, StringReader}

import cats.implicits._
import com.github.tototoshi.csv.{CSVFormat, CSVReader}
import org.monarchinitiative.dosdp.{AxiomType => _, _}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import zio._
import zio.blocking._

import scala.io.Source
import scala.jdk.CollectionConverters._

object Generate extends Logging {

  val LocalLabelProperty: IRI = IRI.create("http://example.org/TSVProvidedLabel")

  def run(config: GenerateConfig): ZIO[ZEnv, DOSDPError, Unit] =
    for {
      axiomsOutput <- ZIO.fromEither(axiomsOutputChoice(config))
      (outputLogicalAxioms, outputAnnotationAxioms) = axiomsOutput
      ontologyOpt <- config.common.ontologyOpt
      prefixes <- config.common.prefixesMap
      sepFormat <- ZIO.fromEither(Config.tabularFormat(config.common.tableFormat))
      axiomSourceProperty <- ZIO.fromOption(Prefixes.idToIRI(config.axiomSourceAnnotationProperty, prefixes).map(AnnotationProperty(_)))
        .orElseFail(DOSDPError("Couldn't create IRI for axiom source annotation property."))
      targets <- determineTargets(config).mapError(e => DOSDPError("Failure to configure input or output", e))
      _ <- ZIO.foreach_(targets) { target =>
        for {
          _ <- logInfo(s"Processing pattern ${target.templateFile}")
          dosdp <- Config.inputDOSDPFrom(target.templateFile)
          columnsAndFillers <- readFillers(new File(target.inputFile), sepFormat)
          (columns, fillers) = columnsAndFillers
          missingColumns = dosdp.allVars.diff(columns)
          _ <- ZIO.foreach_(missingColumns)(c => logWarn(s"Input is missing column for pattern variable <$c>"))
          axioms <- renderPattern(dosdp, prefixes, fillers, ontologyOpt, outputLogicalAxioms, outputAnnotationAxioms, config.restrictAxiomsColumn, config.addAxiomSourceAnnotation.bool, axiomSourceProperty, config.generateDefinedClass.bool)
          _ <- Utilities.saveAxiomsToOntology(axioms, target.outputFile)
        } yield ()
      }
    } yield ()

  def renderPattern(dosdp: DOSDP, prefixes: PartialFunction[String, String], fillers: Map[String, String], ontOpt: Option[OWLOntology], outputLogicalAxioms: Boolean, outputAnnotationAxioms: Boolean, restrictAxiomsColumnName: Option[String], annotateAxiomSource: Boolean, axiomSourceProperty: OWLAnnotationProperty, generateDefinedClass: Boolean): IO[DOSDPError, Set[OWLAxiom]] =
    renderPattern(dosdp, prefixes, List(fillers), ontOpt, outputLogicalAxioms, outputAnnotationAxioms, restrictAxiomsColumnName, annotateAxiomSource, axiomSourceProperty, generateDefinedClass)

  def renderPattern(dosdp: DOSDP, prefixes: PartialFunction[String, String], fillers: List[Map[String, String]], ontOpt: Option[OWLOntology], outputLogicalAxioms: Boolean, outputAnnotationAxioms: Boolean, restrictAxiomsColumnName: Option[String], annotateAxiomSource: Boolean, axiomSourceProperty: OWLAnnotationProperty, generateDefinedClass: Boolean): IO[DOSDPError, Set[OWLAxiom]] = {
    val eDOSDP = ExpandedDOSDP(dosdp, prefixes)
    val readableIDIndex = ontOpt.map(ont => createReadableIdentifierIndex(eDOSDP, ont)).getOrElse(Map.empty)
    val knownColumns = dosdp.allVars
    val generatedAxioms = ZIO.foreach(fillers) { row =>
      val (varBindingsItems, localLabelItems) = (for {
        vars <- dosdp.vars.toSeq
        varr <- vars.keys
        filler <- row.get(varr).flatMap(stripToOption)
        fillerLabelOpt = for {
          fillerIRI <- Prefixes.idToIRI(filler, prefixes)
          label <- row.get(s"${varr}_label").flatMap(stripToOption)
        } yield fillerIRI -> label
      } yield (varr -> SingleValue(filler.trim), fillerLabelOpt)).unzip
      val varBindings = varBindingsItems.toMap
      val localLabels = LocalLabelProperty -> localLabelItems.flatten.toMap
      val listVarBindings = (for {
        listVars <- dosdp.list_vars.toSeq
        listVar <- listVars.keys
        filler <- row.get(listVar).flatMap(stripToOption)
      } yield listVar -> MultiValue(filler.split(DOSDP.MultiValueDelimiter).map(_.trim).to(Set))).toMap
      val dataVarBindings = (for {
        dataVars <- dosdp.data_vars.toSeq
        dataVar <- dataVars.keys
        filler <- row.get(dataVar).flatMap(stripToOption)
      } yield dataVar -> SingleValue(filler.trim)).toMap
      val dataListBindings = (for {
        dataListVars <- dosdp.data_list_vars.toSeq
        dataListVar <- dataListVars.keys
        filler <- row.get(dataListVar).flatMap(stripToOption)
      } yield dataListVar -> MultiValue(filler.split(DOSDP.MultiValueDelimiter).map(_.trim).to(Set))).toMap
      val additionalBindings = for {
        (key, value) <- row.view.filterKeys(k => !knownColumns(k)).toMap
      } yield key -> SingleValue(value.trim)
      val maybeDefinedClass = if (generateDefinedClass) {
        dosdp.pattern_iri.flatMap(id => Prefixes.idToIRI(id, prefixes)).map { patternIRI =>
          val bindingsForDefinedClass = varBindings ++ listVarBindings ++ dataVarBindings ++ dataListBindings
          DOSDP.computeDefinedIRI(patternIRI, bindingsForDefinedClass).toString
        }.toRight(DOSDPError("Pattern must have an IRI if generate-defined-class is requested."))
      } else Right(row(DOSDP.DefinedClassVariable).trim)
      val maybeAxioms = for {
        definedClass <- maybeDefinedClass
        iriBinding = DOSDP.DefinedClassVariable -> SingleValue(definedClass)
        logicalBindings = varBindings + iriBinding
        readableIDIndexPlusLocalLabels = readableIDIndex + localLabels
        initialAnnotationBindings = varBindings.view.mapValues(v => irisToLabels(v, eDOSDP, readableIDIndexPlusLocalLabels)).toMap ++
          listVarBindings.view.mapValues(v => irisToLabels(v, eDOSDP, readableIDIndexPlusLocalLabels)).toMap ++
          dataVarBindings ++
          dataListBindings +
          iriBinding
        annotationBindings = eDOSDP.substitutions.foldLeft(initialAnnotationBindings)((bindings, sub) => sub.expandBindings(bindings)) ++ additionalBindings
        localOutputLogicalAxiomsWithLocalOutputAnnotationAxioms <- restrictAxiomsColumnName.flatMap(column => row.get(column)).map(_.trim).map {
          case "all"        => Right((true, true))
          case "logical"    => Right((true, false))
          case "annotation" => Right((false, true))
          case ""           => Right((outputLogicalAxioms, outputAnnotationAxioms))
          case other        => Left(DOSDPError(s"Invalid value for restrict-axioms-column: $other"))
        }.getOrElse(Right((outputLogicalAxioms, outputAnnotationAxioms)))
        (localOutputLogicalAxioms, localOutputAnnotationAxioms) = localOutputLogicalAxiomsWithLocalOutputAnnotationAxioms
        logicalAxioms = if (localOutputLogicalAxioms) eDOSDP.filledLogicalAxioms(Some(logicalBindings), Some(annotationBindings)) else Set.empty
        annotationAxioms = if (localOutputAnnotationAxioms) eDOSDP.filledAnnotationAxioms(Some(annotationBindings), Some(logicalBindings)) else Set.empty
      } yield logicalAxioms ++ annotationAxioms
      ZIO.fromEither(maybeAxioms)
    }
    generatedAxioms.flatMap { axioms =>
      val allAxioms = axioms.to(Set).flatten
      if (annotateAxiomSource) {
        ZIO.fromOption {
          dosdp.pattern_iri.map(IRI.create).map { patternIRI =>
            allAxioms.map(_.Annotation(axiomSourceProperty, patternIRI))
          }
        }.orElseFail(DOSDPError("Axiom annotations require a value for pattern IRI"))
      }
      else ZIO.succeed(allAxioms)
    }
  }

  private def determineTargets(config: GenerateConfig): ZIO[Blocking, Throwable, List[GenerateTarget]] = {
    val patternNames = config.common.batchPatterns.items
    if (patternNames.nonEmpty) for {
      _ <- logInfo("Running in batch mode")
      _ <- ZIO.ifM(isDirectory(config.common.template))(ZIO.unit,
        ZIO.fail(DOSDPError("\"--template must be a directory in batch mode\"")))
      _ <- ZIO.ifM(isDirectory(config.infile))(ZIO.unit,
        ZIO.fail(DOSDPError("\"--infile must be a directory in batch mode\"")))
      _ <- ZIO.ifM(isDirectory(config.common.outfile))(ZIO.unit,
        ZIO.fail(DOSDPError("\"--outfile must be a directory in batch mode\"")))
    } yield patternNames.map { pattern =>
      val templateFileName = s"${config.common.template}/$pattern.yaml"
      val dataExtension = config.common.tableFormat.toLowerCase
      val dataFileName = s"${config.infile}/$pattern.$dataExtension"
      val outFileName = s"${config.common.outfile}/$pattern.ofn"
      GenerateTarget(templateFileName, dataFileName, outFileName)
    }
    else ZIO.succeed(List(GenerateTarget(config.common.template, config.infile, config.common.outfile)))
  }

  private def isDirectory(path: String): ZIO[Blocking, IOException, Boolean] = effectBlockingIO(new File(path).isDirectory)

  def readFillers(file: File, sepFormat: CSVFormat): ZIO[Blocking, DOSDPError, (Set[String], List[Map[String, String]])] =
    for {
      cleaned <- effectBlockingIO(Source.fromFile(file, "utf-8")).bracketAuto { source =>
        effectBlockingIO(source.getLines().filterNot(_.trim.isEmpty).mkString("\n"))
      }.mapError(e => DOSDPError("Unable to read input table", e))
      columns <- ZIO.effectTotal(CSVReader.open(new StringReader(cleaned))(sepFormat)).bracketAuto { reader =>
        ZIO.effectTotal {
          val iteratorToCheckColumns = reader.iteratorWithHeaders
          if (iteratorToCheckColumns.hasNext) iteratorToCheckColumns.next().keySet else Set.empty[String]
        }
      }
      data <- ZIO.effectTotal(CSVReader.open(new StringReader(cleaned))(sepFormat)).bracketAuto { reader =>
        ZIO.effectTotal(reader.iteratorWithHeaders.toList)
      }
    } yield columns -> data

  private def axiomsOutputChoice(config: GenerateConfig): Either[DOSDPError, (Boolean, Boolean)] =
    config.restrictAxiomsTo match {
      case "all"        => Right((true, true))
      case "logical"    => Right((true, false))
      case "annotation" => Right((false, true))
      case other        => Left(DOSDPError(s"Invalid argument for restrict-axioms-to: $other"))
    }

  private def createReadableIdentifierIndex(dosdp: ExpandedDOSDP, ont: OWLOntology): Map[IRI, Map[IRI, String]] = {
    val properties = dosdp.readableIdentifierProperties.to(Set)
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
    val properties = dosdp.readableIdentifierProperties.map(_.getIRI) ::: LocalLabelProperty :: Nil
    val labelOpt = properties.collectFirst {
      case prop if index.get(prop).exists(_.isDefinedAt(iri)) => index(prop)(iri)
    }
    labelOpt.getOrElse(iri.toString)
  }

  private def stripToOption(text: String): Option[String] = {
    val trimmed = text.trim
    if (trimmed.isEmpty) None else Some(trimmed)
  }

  private final case class GenerateTarget(templateFile: String, inputFile: String, outputFile: String)

}
