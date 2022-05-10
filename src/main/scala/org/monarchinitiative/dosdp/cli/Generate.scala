package org.monarchinitiative.dosdp.cli

import cats.implicits._
import com.github.tototoshi.csv.{CSVFormat, CSVReader}
import org.monarchinitiative.dosdp.cli.Config.{AllAxioms, AnnotationAxioms, AxiomKind, LogicalAxioms}
import org.monarchinitiative.dosdp.cli.DOSDPError.{logError, logErrorFail}
import org.monarchinitiative.dosdp.cli.Main.loggingContext
import org.monarchinitiative.dosdp.{AxiomType => _, _}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import zio._
import zio.blocking._
import zio.logging._

import java.io.{File, StringReader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters._

object Generate {

  val LocalLabelProperty: IRI = IRI.create("http://example.org/TSVProvidedLabel")

  def run(config: GenerateConfig): ZIO[ZEnv with Logging, DOSDPError, Unit] =
    log.locally(_.annotate(loggingContext, Map("command" -> "generate"))) {
      for {
        ontologyOpt <- config.common.ontologyOpt
        prefixes <- config.common.prefixesMap
        (outputLogicalAxioms, outputAnnotationAxioms) = axiomsOutputChoice(config.restrictAxiomsTo)
        sepFormat <- Config.tabularFormat(config.common.tableFormat)
        axiomSourceProperty <- ZIO.fromOption(Prefixes.idToIRI(config.axiomSourceAnnotationProperty, prefixes).map(AnnotationProperty(_)))
          .orElse(logErrorFail("Couldn't create IRI for axiom source annotation property."))
        targets <- determineTargets(config)
        _ <- ZIO.foreach_(targets) { target =>
          log.locally(_.annotate(loggingContext, target.toLogContext)) {
            for {
              _ <- log.info(s"Processing pattern ${target.templateFile}")
              dosdp <- Config.inputDOSDPFrom(target.templateFile)
              columnsAndFillers <- readFillers(new File(target.inputFile), sepFormat)
              (columns, fillers) = columnsAndFillers
              missingColumns = dosdp.allVars.diff(columns.to(Set))
              _ <- ZIO.foreach_(missingColumns)(c => log.warn(s"Input ${target.inputFile} for pattern ${target.templateFile} is missing column for pattern variable <$c>"))
              axioms <- renderPattern(dosdp, prefixes, fillers, ontologyOpt, outputLogicalAxioms, outputAnnotationAxioms, config.restrictAxiomsColumn, config.addAxiomSourceAnnotation.bool, axiomSourceProperty, config.generateDefinedClass.bool, Map.empty)
              _ <- Utilities.saveAxiomsToOntology(axioms, target.outputFile)
            } yield ()
          }
        }
      } yield ()
    }

  def renderPattern(dosdp: DOSDP, prefixes: PartialFunction[String, String], fillers: Map[String, String], ontOpt: Option[OWLOntology], outputLogicalAxioms: Boolean, outputAnnotationAxioms: Boolean, restrictAxiomsColumnName: Option[String], annotateAxiomSource: Boolean, axiomSourceProperty: OWLAnnotationProperty, generateDefinedClass: Boolean, extraReadableIdentifiers: Map[IRI, Map[IRI, String]]): ZIO[Logging, DOSDPError, Set[OWLAxiom]] =
    renderPattern(dosdp, prefixes, List(fillers), ontOpt, outputLogicalAxioms, outputAnnotationAxioms, restrictAxiomsColumnName, annotateAxiomSource, axiomSourceProperty, generateDefinedClass, extraReadableIdentifiers)

  def renderPattern(dosdp: DOSDP, prefixes: PartialFunction[String, String], fillers: List[Map[String, String]], ontOpt: Option[OWLOntology], outputLogicalAxioms: Boolean, outputAnnotationAxioms: Boolean, restrictAxiomsColumnName: Option[String], annotateAxiomSource: Boolean, axiomSourceProperty: OWLAnnotationProperty, generateDefinedClass: Boolean, extraReadableIdentifiers: Map[IRI, Map[IRI, String]]): ZIO[Logging, DOSDPError, Set[OWLAxiom]] = {
    val eDOSDP = ExpandedDOSDP(dosdp, prefixes)
    val knownColumns = dosdp.allVars
    for {
      readableIdentifiers <- eDOSDP.readableIdentifierProperties
      initialReadableIDIndex = ontOpt.map(ont => createReadableIdentifierIndex(readableIdentifiers, eDOSDP, ont)).getOrElse(Map.empty)
      extraReadableIdentifiersInSets = extraReadableIdentifiers.map { case (p, termsToLabel) => p -> termsToLabel.map { case (t, label) => t -> Set(label) } }
      readableIDIndex = (initialReadableIDIndex |+| extraReadableIdentifiersInSets).map { case (p, termsToLabels) => p -> termsToLabels.map { case (t, labels) => t -> labels.toSeq.min } }
      generatedAxioms <- ZIO.foreach(fillers) { row =>
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
        val internalVarBindings = (for {
          internalVars <- dosdp.internal_vars.toSeq
          internalVar <- internalVars
          function <- internalVar.apply.toSeq
          value <- function.apply(Some(dataListBindings.getOrElse(internalVar.input, listVarBindings.getOrElse(internalVar.input, MultiValue(Set.empty[String])))))
        } yield internalVar.var_name -> SingleValue(value)).toMap
        val additionalBindings = for {
          (key, value) <- row.view.filterKeys(k => !knownColumns(k)).toMap
        } yield key -> SingleValue(value.trim)
        val maybeAxioms = for {
          definedClass <- if (generateDefinedClass) {
            ZIO.fromOption(dosdp.pattern_iri.flatMap(id => Prefixes.idToIRI(id, prefixes)).map { patternIRI =>
              val bindingsForDefinedClass = varBindings ++ listVarBindings ++ dataVarBindings ++ dataListBindings ++ internalVarBindings
              DOSDP.computeDefinedIRI(patternIRI, bindingsForDefinedClass).toString
            }).orElse(logErrorFail("Pattern must have an IRI if generate-defined-class is requested."))
          } else ZIO.fromOption(row.get(DOSDP.DefinedClassVariable).map(_.trim))
            .orElse(logErrorFail(s"No input column provided for ${DOSDP.DefinedClassVariable}"))
          iriBinding = DOSDP.DefinedClassVariable -> SingleValue(definedClass)
          logicalBindings = varBindings + iriBinding
          logicalBindingsExtended = logicalBindings ++ listVarBindings
          readableIDIndexPlusLocalLabels = readableIDIndex + localLabels
          initialAnnotationBindings = varBindings.view.mapValues(v => irisToLabels(readableIdentifiers, v, eDOSDP, readableIDIndexPlusLocalLabels)).toMap ++
            listVarBindings.view.mapValues(v => irisToLabels(readableIdentifiers, v, eDOSDP, readableIDIndexPlusLocalLabels)).toMap ++
            internalVarBindings.view.mapValues(v => resolveIrisToLabels(readableIdentifiers, v, eDOSDP, readableIDIndexPlusLocalLabels)).toMap ++
            dataVarBindings ++
            dataListBindings +
            iriBinding
          expandedBindings <- ZIO.foldLeft(eDOSDP.substitutions)(initialAnnotationBindings)((bindings, sub) => sub.expandBindings(bindings))
          annotationBindings = expandedBindings ++ additionalBindings
          localOutputLogicalAxiomsWithLocalOutputAnnotationAxioms <- ZIO.fromEither(restrictAxiomsColumnName.flatMap(column => row.get(column).flatMap(value => stripToOption(value)))
            .map(Config.parseAxiomKind)
            .map(maybeAxiomKind => maybeAxiomKind.map(axiomsOutputChoice))
            .getOrElse(Right((outputLogicalAxioms, outputAnnotationAxioms))))
            .flatMapError(e => logError(s"Malformed value in table restrict-axioms-column: ${e.error}"))
          (localOutputLogicalAxioms, localOutputAnnotationAxioms) = localOutputLogicalAxiomsWithLocalOutputAnnotationAxioms
          logicalAxioms <- if (localOutputLogicalAxioms)
            eDOSDP.filledLogicalAxioms(Some(logicalBindingsExtended), Some(annotationBindings))
          else ZIO.succeed(Set.empty)
          annotationAxioms <- if (localOutputAnnotationAxioms)
            eDOSDP.filledAnnotationAxioms(Some(annotationBindings), Some(logicalBindingsExtended))
          else ZIO.succeed(Set.empty)
        } yield logicalAxioms ++ annotationAxioms
        maybeAxioms
      }
      allAxioms = generatedAxioms.to(Set).flatten
      res <- if (annotateAxiomSource) {
        ZIO.fromOption {
          dosdp.pattern_iri.map(IRI.create).map { patternIRI =>
            allAxioms.map(_.Annotation(axiomSourceProperty, patternIRI))
          }
        }.orElseFail("Axiom annotations require a value for pattern IRI").flatMapError(e => logError(e))
      }
      else ZIO.succeed(allAxioms)
    } yield res
  }

  private def determineTargets(config: GenerateConfig): ZIO[Blocking with Logging, DOSDPError, List[GenerateTarget]] = {
    val patternNames = config.common.batchPatterns.items
    if (patternNames.nonEmpty) for {
      _ <- log.info("Running in batch mode")
      _ <- ZIO.foreach_(patternNames)(pattern => ZIO.when(!Files.exists(Paths.get(config.common.template, s"$pattern.yaml")))(logErrorFail(s"Pattern doesn't exist: $pattern")))
      _ <- ZIO.when(!Files.isDirectory(Paths.get(config.common.template)))(logErrorFail("\"--template must be a directory in batch mode\""))
      _ <- ZIO.when(!Files.isDirectory(Paths.get(config.infile)))(logErrorFail("\"--infile must be a directory in batch mode\""))
      _ <- ZIO.when(!Files.isDirectory(Paths.get(config.common.outfile)))(logErrorFail("\"--outfile must be a directory in batch mode\""))
    } yield patternNames.map { pattern =>
      val templateFileName = s"${config.common.template}/$pattern.yaml"
      val dataExtension = config.common.tableFormat.toLowerCase
      val dataFileName = s"${config.infile}/$pattern.$dataExtension"
      val outFileName = s"${config.common.outfile}/$pattern.ofn"
      GenerateTarget(templateFileName, dataFileName, outFileName)
    }
    else ZIO.succeed(List(GenerateTarget(config.common.template, config.infile, config.common.outfile)))
  }

  def readFillers(file: File, sepFormat: CSVFormat): ZIO[Blocking with Logging, DOSDPError, (Seq[String], List[Map[String, String]])] =
    for {
      cleaned <- effectBlockingIO(Source.fromFile(file, StandardCharsets.UTF_8.name())).bracketAuto { source =>
        effectBlockingIO(source.getLines().filterNot(_.trim.isEmpty).mkString("\n"))
      }.flatMapError(e => logError("Unable to read input table", e))
      columns <- ZIO.effectTotal(CSVReader.open(new StringReader(cleaned))(sepFormat)).bracketAuto { reader =>
        ZIO.effectTotal {
          val iteratorToCheckColumns = reader.iteratorWithHeaders
          if (iteratorToCheckColumns.hasNext) iteratorToCheckColumns.next().keys.to(Seq) else Seq.empty[String]
        }
      }
      data <- ZIO.effectTotal(CSVReader.open(new StringReader(cleaned))(sepFormat)).bracketAuto { reader =>
        ZIO.effectTotal(reader.iteratorWithHeaders.toList)
      }
    } yield columns -> data

  def axiomsOutputChoice(kind: AxiomKind): (Boolean, Boolean) = kind match {
    case AllAxioms        => (true, true)
    case LogicalAxioms    => (true, false)
    case AnnotationAxioms => (false, true)
  }

  private def createReadableIdentifierIndex(readableIdentifiers: List[OWLAnnotationProperty], dosdp: ExpandedDOSDP, ont: OWLOntology): Map[IRI, Map[IRI, Set[String]]] = {
    val properties = readableIdentifiers.to(Set)
    val mappings = for {
      AnnotationAssertion(_, prop, subj: IRI, value ^^ _) <- ont.getAxioms(AxiomType.ANNOTATION_ASSERTION, Imports.INCLUDED).asScala
      if properties(prop)
    } yield Map(prop.getIRI -> Map(subj -> Set(value)))
    mappings.fold(Map.empty)(_ combine _)
  }

  private def irisToLabels(readableIdentifiers: List[OWLAnnotationProperty], binding: Binding, dosdp: ExpandedDOSDP, index: Map[IRI, Map[IRI, String]]): Binding = binding match {
    case SingleValue(value) => SingleValue(Prefixes.idToIRI(value, dosdp.prefixes).map(iri => readableIdentifierForIRI(readableIdentifiers, iri, dosdp, index)).getOrElse(value))
    case MultiValue(values) => MultiValue(values.map(value => Prefixes.idToIRI(value, dosdp.prefixes).map(iri => readableIdentifierForIRI(readableIdentifiers, iri, dosdp, index)).getOrElse(value)))
  }

  private def resolveIrisToLabels(readableIdentifiers: List[OWLAnnotationProperty], binding: SingleValue, dosdp: ExpandedDOSDP, index: Map[IRI, Map[IRI, String]]): Binding = {
    val CURIEList = "([^ ,:]*):([^ ,]*)".r
    val CURIEListEmbed = CURIEList.unanchored
    val value = binding.value
    var resolvedValue = value
    if (CURIEListEmbed.matches(value)) {
      CURIEList.findAllMatchIn(value).foreach(matching => dosdp.prefixes.lift(matching.group(1)).map(uri =>
        resolvedValue = resolvedValue.replaceFirst(matching.group(1) + ":" + matching.group(2),
          readableIdentifierForIRI(readableIdentifiers, IRI.create(uri + matching.group(2)), dosdp, index))))
    }
    SingleValue(resolvedValue)
  }

  private def readableIdentifierForIRI(readableIdentifiers: List[OWLAnnotationProperty], iri: IRI, dosdp: ExpandedDOSDP, index: Map[IRI, Map[IRI, String]]): String = {
    val properties = readableIdentifiers.map(_.getIRI) ::: LocalLabelProperty :: Nil
    val labelOpt = properties.collectFirst {
      case prop if index.get(prop).exists(_.isDefinedAt(iri)) => index(prop)(iri)
    }
    labelOpt.getOrElse(iri.toString)
  }

  private def stripToOption(text: String): Option[String] = {
    val trimmed = text.trim
    if (trimmed.isEmpty) None else Some(trimmed)
  }

  final case class GenerateTarget(templateFile: String, inputFile: String, outputFile: String) {

    def toLogContext: Map[String, String] = Map("pattern" -> templateFile, "input" -> inputFile, "output" -> outputFile)

  }

}
