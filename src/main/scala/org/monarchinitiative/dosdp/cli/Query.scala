package org.monarchinitiative.dosdp.cli

import com.github.tototoshi.csv.CSVWriter
import org.apache.jena.query.{QueryExecutionFactory, QueryFactory, QuerySolution}
import org.apache.jena.rdf.model.{Model, ModelFactory}
import org.apache.jena.vocabulary.DCTerms
import org.monarchinitiative.dosdp.cli.Config.AxiomKind
import org.monarchinitiative.dosdp.{DOSDP, ExpandedDOSDP, SPARQL, SesameJena}
import org.phenoscape.owlet.Owlet
import org.phenoscape.scowl._
import org.semanticweb.HermiT.ReasonerFactory
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.TurtleDocumentFormat
import org.semanticweb.owlapi.model.{IRI, OWLAnnotationAssertionAxiom, OWLAxiom, OWLOntology}
import org.semanticweb.owlapi.reasoner.{OWLReasoner, OWLReasonerFactory}
import uk.ac.manchester.cs.jfact.JFactFactory
import zio._
import zio.blocking.Blocking

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file._
import scala.jdk.CollectionConverters._

object Query {

  private val ConformsTo = AnnotationProperty(DCTerms.conformsTo.getURI)

  def run(config: QueryConfig): ZIO[ZEnv, DOSDPError, Unit] = {
    val reasonerFactoryOptZ = ZIO.foreach(config.reasoner) { reasonerArg =>
      reasonerArg.toLowerCase match {
        case "elk"    => ZIO.succeed(new ElkReasonerFactory())
        case "hermit" => ZIO.succeed(new ReasonerFactory())
        case "jfact"  => ZIO.succeed(new JFactFactory())
        case other    => ZIO.fail(DOSDPError(s"Reasoner $other not supported. Options are ELK, HermiT, or JFact"))
      }
    }
    for {
      targets <- determineTargets(config)
      reasonerFactoryOpt <- reasonerFactoryOptZ
      ontologyOpt <- config.common.ontologyOpt
      modelOpt <- ZIO.foreach(ontologyOpt)(makeModel)
      allConformanceAnnotations <- makeOptionalReasoner(ontologyOpt, reasonerFactoryOpt).use { reasonerOpt =>
        ZIO.foreachParN(config.parallelism)(targets) { target =>
          for {
            _ <- ZIO.effectTotal(scribe.info(s"Processing pattern ${target.templateFile}"))
            queryAndOptionalPatternIRI <- createQueryWithPatternIRI(target, config, reasonerOpt)
            (queryString, patternIRIOpt) = queryAndOptionalPatternIRI
            conformanceAnnotations <- processTarget(target, config, queryString, modelOpt, patternIRIOpt)
          } yield conformanceAnnotations
        }.map(_.to(Set).flatten)
      }
      _ <- ZIO.foreach_(config.outputConformance)(writeConformanceFile(_, allConformanceAnnotations))
    } yield ()
  }

  def makeModel(ont: OWLOntology): IO[DOSDPError, Model] =
    for {
      model <- ZIO.effectTotal(ModelFactory.createDefaultModel())
      allAxioms = for {
        completeOnt <- ont.getImportsClosure.asScala.to(Set)
        axiom <- completeOnt.getAxioms().asScala.to(Set)
      } yield axiom
      manager <- ZIO.effectTotal(OWLManager.createOWLOntologyManager())
      finalOnt <- ZIO.effectTotal(manager.createOntology(allAxioms.asJava))
      triples = SesameJena.ontologyAsTriples(finalOnt)
      _ <- ZIO.effectTotal(model.add(triples.toList.asJava))
    } yield model

  private def makeOptionalReasoner(ontologyOpt: Option[OWLOntology], factoryOpt: Option[OWLReasonerFactory]): Managed[DOSDPError, Option[OWLReasoner]] =
    ZManaged.foreach(
      for {
        ontology <- ontologyOpt
        factory <- factoryOpt
      } yield ZIO
        .effect(factory.createReasoner(ontology))
        .mapError(e => DOSDPError(s"Failed to create reasoner for ontology $ontology", e))
        .toManaged(o => ZIO.effectTotal(o.dispose()))
    )(identity)

  private def createQueryWithPatternIRI(target: QueryTarget, config: QueryConfig, reasonerOpt: Option[OWLReasoner]): IO[DOSDPError, (String, Option[String])] =
    for {
      dosdp <- Config.inputDOSDPFrom(target.templateFile)
      prefixes <- config.common.prefixesMap
      query <- ZIO.fromEither(makeProcessedQuery(dosdp, prefixes, config.restrictAxiomsTo, reasonerOpt))
    } yield (query, dosdp.pattern_iri)

  def makeProcessedQuery(dosdp: DOSDP, prefixes: PartialFunction[String, String], axiomKind: AxiomKind, reasonerOpt: Option[OWLReasoner]): Either[DOSDPError, String] = {
    val maybeSparqlQuery = SPARQL.queryFor(ExpandedDOSDP(dosdp, prefixes), axiomKind)
    for {
      sparqlQuery <- maybeSparqlQuery
    } yield {
      reasonerOpt
        .map { reasoner =>
          new Owlet(reasoner).expandQueryString(sparqlQuery, asValues = true)
        }
        .getOrElse(sparqlQuery)
    }
  }

  private def processTarget(target: QueryTarget,
                            config: QueryConfig,
                            processedQuery: String,
                            modelOpt: Option[Model],
                            patternIRIOpt: Option[String]): IO[DOSDPError, Set[OWLAnnotationAssertionAxiom]] = {
    val doPrintQuery = ZIO
      .effect(new PrintWriter(new File(target.outputFile), StandardCharsets.UTF_8.name()))
      .bracketAuto(w => ZIO.effect(w.print(processedQuery)))
    val doPerformQuery = for {
      model <- ZIO.fromOption(modelOpt).orElseFail(DOSDPError("Can't run query; no ontology provided."))
      (columns, results) <- performQuery(processedQuery, model)
      sepFormat <- ZIO.fromEither(Config.tabularFormat(config.common.tableFormat))
      conformanceAnnotations <- ZIO.foreach(patternIRIOpt) { patternIRI =>
        val pattern = IRI.create(patternIRI)
        ZIO.effect(results.map(_.get(DOSDP.DefinedClassVariable)).map(_.asResource().getURI).map(IRI.create)).map { matchingDefinedClasses =>
          matchingDefinedClasses.map(AnnotationAssertion(ConformsTo, _, pattern))
        }
      }
      _ <-
        ZIO
          .effect(CSVWriter.open(target.outputFile, StandardCharsets.UTF_8.name())(sepFormat))
          .bracketAuto(w => writeQueryResults(w, columns, results))
    } yield conformanceAnnotations.toList.flatten.to(Set)
    (if (config.printQuery.bool) doPrintQuery.as(Set.empty[OWLAnnotationAssertionAxiom]) else doPerformQuery).mapError(e => DOSDPError("Failure performing query command", e))
  }

  private def writeQueryResults(writer: CSVWriter, columns: List[String], results: List[QuerySolution]): IO[Throwable, List[Unit]] =
    ZIO.effect(writer.writeRow(columns)) *> ZIO.foreach(results) { qs =>
      ZIO.effect(writer.writeRow(columns.map(variable => Option(qs.get(variable)).map(_.toString).getOrElse(""))))
    }

  private def determineTargets(config: QueryConfig): ZIO[Blocking, DOSDPError, List[QueryTarget]] = {
    val patternNames = config.common.batchPatterns.items
    if (patternNames.nonEmpty) for {
      _ <- ZIO.effectTotal(scribe.info("Running in batch mode"))
      _ <- ZIO.foreach_(patternNames)(pattern => ZIO.when(!Files.exists(Paths.get(config.common.template, s"$pattern.yaml")))(ZIO.fail(DOSDPError(s"Pattern file doesn't exist: $pattern"))))
      _ <- ZIO.when(!Files.exists(Paths.get(config.common.template)))(ZIO.fail(DOSDPError("\"--template must be a directory in batch mode\"")))
      _ <- ZIO.when(!Files.exists(Paths.get(config.common.outfile)))(ZIO.fail(DOSDPError("\"--outfile must be a directory in batch mode\"")))
    } yield patternNames.map { pattern =>
      val templateFileName = s"${config.common.template}/$pattern.yaml"
      val suffix =
        if (config.printQuery.bool) "rq"
        else config.common.tableFormat.toLowerCase
      val outFileName = s"${config.common.outfile}/$pattern.$suffix"
      QueryTarget(templateFileName, outFileName)
    }
    else ZIO.succeed(List(QueryTarget(config.common.template, config.common.outfile)))
  }

  def performQuery(sparql: String, model: Model): Task[(List[String], List[QuerySolution])] =
    for {
      query <- ZIO.effect(QueryFactory.create(sparql))
      results <- ZIO.effect(QueryExecutionFactory.create(query, model)).bracketAuto { qe =>
        ZIO.effect {
          val resultSet = qe.execSelect()
          val columns = resultSet.getResultVars.asScala.toList
          val results = resultSet.asScala.toList
          (columns, results)
        }
      }
    } yield results

  def writeConformanceFile(path: String, annotations: Set[OWLAnnotationAssertionAxiom]): IO[DOSDPError, Unit] =
    (for {
      manager <- ZIO.effect(OWLManager.createOWLOntologyManager())
      ont <- ZIO.effect(manager.createOntology(annotations.toSet[OWLAxiom].asJava))
      _ <- ZIO.effect(manager.saveOntology(ont, new TurtleDocumentFormat(), IRI.create(new File(path))))
    } yield ()).mapError(DOSDPError(s"Failed to write pattern conformance file to path $path", _))

  final private case class QueryTarget(templateFile: String, outputFile: String)

}
