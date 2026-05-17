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
    val knownColumns = dosdp.allVars
    for {
      _ <- ZIO.when(generateDefinedClass && fillers.exists(_.contains(DOSDP.DefinedClassVariable)))(
        logErrorFail(s"Input table must not have a '${DOSDP.DefinedClassVariable}' column when --generate-defined-class is requested."))
      compiled <- PatternCompiler.compile(dosdp, prefixes)
      permutationProperties = compiled.permutationProperties
      permutationIndex =
        if (permutationProperties.isEmpty) Map.empty[IRI, Map[IRI, Set[String]]]
        else ontOpt.map(createPermutationIndex(_, permutationProperties)).getOrElse(Map.empty)
      readableIdentifiers = compiled.readableIdentifierProperties
      initialReadableIDIndex = ontOpt.map(ont => createReadableIdentifierIndex(readableIdentifiers, ont)).getOrElse(Map.empty)
      extraReadableIdentifiersInSets = extraReadableIdentifiers.map { case (p, termsToLabel) => p -> termsToLabel.map { case (t, label) => t -> Set(label) } }
      readableIDIndex = (initialReadableIDIndex |+| extraReadableIdentifiersInSets).map { case (p, termsToLabels) => p -> termsToLabels.map { case (t, labels) => t -> labels.toSeq.min } }
      expansionContext = Expansion.ExpansionContext(
        readableIDIndex = readableIDIndex,
        permutationIndex = permutationIndex,
        outputLogicalAxioms = outputLogicalAxioms,
        outputAnnotationAxioms = outputAnnotationAxioms,
        restrictAxiomsColumn = restrictAxiomsColumnName,
        generateDefinedClass = generateDefinedClass,
        readableIdentifiers = readableIdentifiers,
        localLabelProperty = LocalLabelProperty)
      generatedAxioms <- ZIO.foreach(fillers) { row =>
        val bindings = RowBindings.fromRow(dosdp, prefixes, knownColumns, row)
        ZIO.fromEither(Expansion.expandRow(compiled, bindings, row, expansionContext))
          .flatMapError(e => logError(e.message))
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

  private def createReadableIdentifierIndex(readableIdentifiers: List[OWLAnnotationProperty], ont: OWLOntology): Map[IRI, Map[IRI, Set[String]]] = {
    val properties = readableIdentifiers.to(Set)
    val mappings = for {
      AnnotationAssertion(_, prop, subj: IRI, value ^^ _) <- ont.getAxioms(AxiomType.ANNOTATION_ASSERTION, Imports.INCLUDED).asScala
      if properties(prop)
    } yield Map(prop.getIRI -> Map(subj -> Set(value)))
    mappings.fold(Map.empty)(_ combine _)
  }

  /**
   * Index of annotation property values for filler terms, used for permutation generation.
   * Only the supplied `properties` are indexed — every other annotation assertion in the
   * ontology is skipped.
   * Structure: Map[FillerTermIRI, Map[AnnotationPropertyIRI, Set[AnnotationValues]]]
   */
  private def createPermutationIndex(ont: OWLOntology, properties: Set[OWLAnnotationProperty]): Map[IRI, Map[IRI, Set[String]]] = {
    val mappings = for {
      AnnotationAssertion(_, prop, subj: IRI, value ^^ _) <- ont.getAxioms(AxiomType.ANNOTATION_ASSERTION, Imports.INCLUDED).asScala
      if properties(prop)
    } yield Map(subj -> Map(prop.getIRI -> Set(value)))
    mappings.fold(Map.empty)(_ combine _)
  }

  private def stripToOption(text: String): Option[String] = {
    val trimmed = text.trim
    if (trimmed.isEmpty) None else Some(trimmed)
  }

  final case class GenerateTarget(templateFile: String, inputFile: String, outputFile: String) {

    def toLogContext: Map[String, String] = Map("pattern" -> templateFile, "input" -> inputFile, "output" -> outputFile)

  }

  /**
   * Per-row binding maps extracted from a TSV row, grouped by the kind of pattern
   * variable they fill. The split mirrors the four variable dictionaries in DOSDP
   * (`vars` / `list_vars` / `data_vars` / `data_list_vars`) plus the derived
   * `internal_vars` and any extra columns the user declared but did not bind to
   * a variable dictionary.
   *
   * `additionalBindings` deliberately excludes the `defined_class` column. The
   * iri-binding that downstream code constructs from the resolved defined-class
   * IRI must remain authoritative for both logical and annotation axioms, so a
   * stray `defined_class` column must not be allowed to leak in via
   * `annotationBindings = expandedBindings ++ additionalBindings` and override
   * it.
   *
   * `localLabels` captures `<var>_label` columns — user-supplied display labels
   * that are applied during annotation rendering ahead of any ontology lookup.
   */
  private[dosdp] final case class RowBindings(
    varBindings: Map[String, SingleValue],
    listVarBindings: Map[String, MultiValue],
    dataVarBindings: Map[String, SingleValue],
    dataListBindings: Map[String, MultiValue],
    internalVarBindings: Map[String, SingleValue],
    additionalBindings: Map[String, SingleValue],
    localLabels: Map[IRI, String]
  ) {

    /** All bindings that participate in the computed defined-class IRI hash. */
    def bindingsForDefinedClassIRI: Map[String, Binding] =
      varBindings ++ listVarBindings ++ dataVarBindings ++ dataListBindings ++ internalVarBindings

  }

  private[dosdp] object RowBindings {

    private def collect[V <: Binding](dict: Option[Map[String, String]], row: Map[String, String])(mkBinding: String => V): Map[String, V] =
      dict.toSeq.flatMap(_.keys).flatMap(name => row.get(name).flatMap(stripToOption).map(name -> mkBinding(_))).toMap

    private def parseMultiValue(filler: String): MultiValue =
      MultiValue(filler.split(DOSDP.MultiValueDelimiter).map(_.trim).to(Set))

    /**
     * Synthetic placeholder row: binds every declared variable to its
     * `urn:dosdp:` IRI (class / list vars) or `$<name>` literal placeholder
     * (data / data-list vars). Feeding this row to `Expansion.expandRow`
     * leaves every variable slot in `compiled.parsed` untouched (each
     * substitution maps the placeholder to itself), so the resulting axioms
     * are byte-equivalent to what `ExpandedDOSDP.filledLogicalAxioms` and
     * `filledAnnotationAxioms(varsOnlyIRIBindings, None)` produce — the
     * placeholder-form output that drives `query` and `terms`.
     *
     * The companion `defined_class` placeholder lives in the row map
     * (see `placeholderRow`) rather than here, so the existing
     * `resolveDefinedClass` path resolves it like any other row.
     */
    def placeholder(dosdp: DOSDP): RowBindings = {
      def placeholderIRI(name: String): String = DOSDP.variableToIRI(name).toString
      def placeholderLiteral(name: String): String = "$" + name
      val varBindings = dosdp.vars.getOrElse(Map.empty).keys
        .map(name => name -> SingleValue(placeholderIRI(name))).toMap
      val listVarBindings = dosdp.list_vars.getOrElse(Map.empty).keys
        .map(name => name -> MultiValue(Set(placeholderIRI(name)))).toMap
      val dataVarBindings = dosdp.data_vars.getOrElse(Map.empty).keys
        .map(name => name -> SingleValue(placeholderLiteral(name))).toMap
      val dataListBindings = dosdp.data_list_vars.getOrElse(Map.empty).keys
        .map(name => name -> MultiValue(Set(placeholderLiteral(name)))).toMap
      RowBindings(varBindings, listVarBindings, dataVarBindings, dataListBindings,
        Map.empty, Map.empty, Map.empty)
    }

    /**
     * Row map paired with `placeholder(dosdp)`: supplies the `defined_class`
     * placeholder so `resolveDefinedClass` returns the pattern's
     * `urn:dosdp:defined_class` IRI without taking the row-missing-column
     * path.
     */
    def placeholderRow: Map[String, String] =
      Map(DOSDP.DefinedClassVariable -> DOSDP.variableToIRI(DOSDP.DefinedClassVariable).toString)

    def fromRow(dosdp: DOSDP, prefixes: PartialFunction[String, String], knownColumns: Set[String], row: Map[String, String]): RowBindings = {
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
      val localLabels = localLabelItems.flatten.toMap
      val listVarBindings = collect(dosdp.list_vars, row)(parseMultiValue)
      val dataVarBindings = collect(dosdp.data_vars, row)(s => SingleValue(s.trim))
      val dataListBindings = collect(dosdp.data_list_vars, row)(parseMultiValue)
      val internalVarBindings = (for {
        internalVars <- dosdp.internal_vars.toSeq
        internalVar <- internalVars
        function <- internalVar.apply.toSeq
        value <- function.apply(Some(dataListBindings.getOrElse(internalVar.input, listVarBindings.getOrElse(internalVar.input, MultiValue(Set.empty[String])))))
      } yield internalVar.var_name -> SingleValue(value)).toMap
      val additionalBindings = (for {
        (key, value) <- row.view.filterKeys(k => !knownColumns(k) && k != DOSDP.DefinedClassVariable).toMap
      } yield key -> SingleValue(value.trim)).toMap
      RowBindings(varBindings, listVarBindings, dataVarBindings, dataListBindings,
        internalVarBindings, additionalBindings, localLabels)
    }

  }

}
