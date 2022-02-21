package org.monarchinitiative.dosdp.cli

import com.github.tototoshi.csv.CSVFormat
import org.geneontology.owl.differ.ManchesterSyntaxOWLObjectRenderer
import org.geneontology.owl.differ.shortform.MarkdownLinkShortFormProvider
import org.monarchinitiative.dosdp.cli.DOSDPError.{logError, logErrorFail}
import org.monarchinitiative.dosdp.cli.Generate.readFillers
import org.monarchinitiative.dosdp.cli.Main.loggingContext
import org.monarchinitiative.dosdp.cli.Prototype.OboInOwlSource
import org.monarchinitiative.dosdp.{DOSDP, DocsMarkdown, ExpandedDOSDP, Prefixes}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.OWLObjectRenderer
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider
import zio._
import zio.blocking._
import zio.logging._

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

object Docs {

  private val Definition = AnnotationProperty("http://purl.obolibrary.org/obo/IAO_0000115")
  private val Special = Set(RDFSLabel, Definition)

  def run(config: DocsConfig): ZIO[ZEnv with Logging, DOSDPError, Unit] = {
    log.locally(_.annotate(loggingContext, Map("command" -> "docs"))) {
      for {
        sepFormat <- Config.tabularFormat(config.common.tableFormat)
        targets <- determineTargets(config)
        ontologyOpt <- config.common.ontologyOpt
        ontology = ontologyOpt.getOrElse(OWLManager.createOWLOntologyManager().createOntology())
        _ <- ZIO.foreach_(targets)(processTarget(_, sepFormat, config, ontology))
        _ <- writeIndex(targets, config.common.outfile).when(config.common.batchPatterns.items.nonEmpty)
      } yield ()
    }
  }

  private def processTarget(target: DocsTarget, sepFormat: CSVFormat, config: DocsConfig, ontology: OWLOntology): ZIO[Blocking with Logging, DOSDPError, Unit] =
    log.locally(_.annotate(loggingContext, target.toLogContext)) {
      for {
        _ <- log.info(s"Processing pattern ${target.templateFile}")
        prefixes <- config.common.prefixesMap
        dosdp <- Config.inputDOSDPFrom(target.templateFile)
        eDOSDP = ExpandedDOSDP(dosdp, prefixes)
        columnsAndFillers <- readFillers(new File(target.inputFile), sepFormat)
        (columns, rows) = columnsAndFillers
        prefixes <- config.common.prefixesMap
        iri <- ZIO.fromOption(dosdp.pattern_iri).orElse(logErrorFail("Pattern must have pattern IRI for document command"))
        fillers = dosdp.vars.getOrElse(Map.empty).map { case (k, _) => k -> s"http://dosdp.org/filler/$k" } ++
          dosdp.list_vars.getOrElse(Map.empty).map { case (k, _) => k -> s"http://dosdp.org/filler/$k" } ++
          dosdp.data_vars.getOrElse(Map.empty).map { case (k, _) => k -> s"`{$k}`" } ++
          dosdp.data_list_vars.getOrElse(Map.empty).map { case (k, _) => k -> s"`{$k}`" } +
          (DOSDP.DefinedClassVariable -> iri)
        variableReadableIdentifiers = (dosdp.vars.getOrElse(Map.empty).map { case (k, _) => k -> s"http://dosdp.org/filler/$k" } ++
          dosdp.list_vars.getOrElse(Map.empty).map { case (k, _) => k -> s"http://dosdp.org/filler/$k" }).map(e => IRI.create(e._2) -> s"`{${e._1}}`")
        renderer = objectRenderer(ontology, variableReadableIdentifiers)
        axioms <- Generate.renderPattern(dosdp, prefixes, fillers, Some(ontology), true, true, None, false, OboInOwlSource, false, Map(RDFSLabel.getIRI -> variableReadableIdentifiers))
        patternIRI = IRI.create(iri)
        docAxioms = findDocAxioms(patternIRI, axioms, target, config.dataLocationPrefix)
        data = columns.to(List) :: rows.take(5).map(formatDataRow(_, columns.to(List), prefixes)) ::: Nil
        markdown <- DocsMarkdown.markdown(eDOSDP, docAxioms, renderer, data)
        _ <- effectBlockingIO(new PrintWriter(target.outputFile, "utf-8")).bracketAuto { writer =>
          effectBlockingIO(writer.print(markdown))
        }.flatMapError(e => logError(s"Couldn't write Markdown to file ${target.outputFile}", e))
      } yield ()
    }

  private def writeIndex(targets: List[DocsTarget], outpath: String) = {
    for {
      dosdpsAndOutfiles <- ZIO.foreach(targets) { target =>
        Config.inputDOSDPFrom(target.templateFile).map(_ -> new File(target.outputFile).getName)
      }
      markdown = DocsMarkdown.indexMarkdown(dosdpsAndOutfiles)
      _ <- effectBlockingIO(new PrintWriter(s"$outpath/index.md", "utf-8")).bracketAuto { writer =>
        effectBlockingIO(writer.print(markdown))
      }.flatMapError(e => logError(s"Couldn't write Markdown to file $outpath/index.md", e))
    } yield DocsMarkdown.indexMarkdown(dosdpsAndOutfiles)
  }

  private def determineTargets(config: DocsConfig): ZIO[Blocking with Logging, DOSDPError, List[DocsTarget]] = {
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
      val outFileName = s"${config.common.outfile}/$pattern.md"
      DocsTarget(templateFileName, dataFileName, outFileName)
    }
    else ZIO.succeed(List(DocsTarget(config.common.template, config.infile, config.common.outfile)))
  }

  //TODO better handling for list values?
  private def formatDataRow(row: Map[String, String], columns: List[String], prefixes: PartialFunction[String, String]): List[String] =
    columns.map(c => row.getOrElse(c, "")).map(v => Prefixes.idToIRI(v, prefixes).map(iri => s"[$v]($iri)").getOrElse(v))

  private final case class DocsTarget(templateFile: String, inputFile: String, outputFile: String) {

    def toLogContext: Map[String, String] = Map("pattern" -> templateFile, "input" -> inputFile, "output" -> outputFile)

  }

  final case class DocData(
                            name: Set[OWLAnnotationAssertionAxiom],
                            definition: Set[OWLAnnotationAssertionAxiom],
                            annotations: Set[OWLAnnotationAssertionAxiom],
                            equivalentTo: Set[OWLEquivalentClassesAxiom],
                            subClassOf: Set[OWLSubClassOfAxiom],
                            otherAxioms: Set[OWLAxiom],
                            dataLocation: String
                          )

  private def findDocAxioms(namedClassIRI: IRI, axioms: Set[OWLAxiom], target: DocsTarget, dataLocationPrefix: String): DocData = {
    val PatternIRI = namedClassIRI
    val PatternCls = Class(namedClassIRI)
    val names = axioms.collect { case ax @ AnnotationAssertion(_, RDFSLabel, PatternIRI, _) => ax }
    val definitions = axioms.collect { case ax @ AnnotationAssertion(_, Definition, PatternIRI, _) => ax }
    val annotations = axioms.collect { case ax @ AnnotationAssertion(_, prop, PatternIRI, _) if !Special(prop) => ax }
    val equivalentTo = axioms.collect { case ax @ EquivalentClasses(_, clss) if clss.toSet[OWLClassExpression](PatternCls) => ax }
    val subClassOf = axioms.collect { case ax @ SubClassOf(_, PatternCls, _) => ax }
    val other = axioms -- names -- definitions -- annotations -- equivalentTo -- subClassOf
    val dataFileName = new File(target.inputFile).getName
    val dataLocation = s"$dataLocationPrefix$dataFileName"
    DocData(names, definitions, annotations, equivalentTo, subClassOf, other, dataLocation)
  }

  def objectRenderer(ont: OWLOntology, extraReadableIdentifiers: Map[IRI, String]): OWLObjectRenderer = {
    val labelProvider = new AnnotationValueShortFormProvider(List(RDFSLabel).asJava, Map.empty[OWLAnnotationProperty, java.util.List[String]].asJava, ont.getOWLOntologyManager) {
      override def getShortForm(entity: OWLEntity): String = extraReadableIdentifiers.getOrElse(entity.getIRI, super.getShortForm(entity))
    }
    val markdownLinkProvider = new MarkdownLinkShortFormProvider(labelProvider) {
      override def getShortForm(entity: OWLEntity): String =
        if (entity.getIRI.toString.startsWith("http://dosdp.org/filler/")) labelProvider.getShortForm(entity)
        else super.getShortForm(entity)
    }
    val renderer = new ManchesterSyntaxOWLObjectRenderer()
    renderer.setShortFormProvider(markdownLinkProvider)
    renderer
  }

}
