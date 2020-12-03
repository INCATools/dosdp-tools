package org.monarchinitiative.dosdp.cli

import java.io.{File, PrintWriter}

import com.github.tototoshi.csv.CSVWriter
import org.apache.jena.query.{QueryExecutionFactory, QueryFactory, QuerySolution}
import org.apache.jena.rdf.model.{Model, ModelFactory}
import org.monarchinitiative.dosdp.Utilities.isDirectory
import org.monarchinitiative.dosdp.cli.Config.AxiomKind
import org.monarchinitiative.dosdp.{DOSDP, ExpandedDOSDP, SPARQL, SesameJena}
import org.phenoscape.owlet.Owlet
import org.semanticweb.HermiT.ReasonerFactory
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.reasoner.{OWLReasoner, OWLReasonerFactory}
import uk.ac.manchester.cs.jfact.JFactFactory
import zio._
import zio.blocking.Blocking

import scala.jdk.CollectionConverters._

object Query {

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
      targets <- determineTargets(config).mapError(e => DOSDPError("Failure to configure input or output", e))
      reasonerFactoryOpt <- reasonerFactoryOptZ
      ontologyOpt <- config.common.ontologyOpt
      model <- makeModel(ontologyOpt)
      _ <- makeOptionalReasoner(ontologyOpt, reasonerFactoryOpt).use { reasonerOpt =>
        ZIO.foreach(targets) { target =>
          ZIO.effectTotal(scribe.info(s"Processing pattern ${target.templateFile}")) *>
            createQuery(target, config, reasonerOpt).flatMap(processTarget(target, config, _, model))
        }
      }
    } yield ()
  }

  def makeModel(ontologyOpt: Option[OWLOntology]): ZIO[Any, DOSDPError, Option[Model]] =
    for {
      model <- ZIO.effectTotal(ModelFactory.createDefaultModel())
      ont <- ZIO.fromOption(ontologyOpt).orElseFail(DOSDPError("Can't run query; no ontology provided."))
      allAxioms = for {
        completeOnt <- ont.getImportsClosure.asScala.to(Set)
        axiom <- completeOnt.getAxioms().asScala.to(Set)
      } yield axiom
      manager <- ZIO.effectTotal(OWLManager.createOWLOntologyManager())
      finalOnt <- ZIO.effectTotal(manager.createOntology(allAxioms.asJava))
      triples = SesameJena.ontologyAsTriples(finalOnt)
      _ <- ZIO.effectTotal(model.add(triples.toList.asJava))
    } yield Some(model)

  private def makeOptionalReasoner(ontologyOpt: Option[OWLOntology], factoryOpt: Option[OWLReasonerFactory]): ZManaged[Any, DOSDPError, Option[OWLReasoner]] =
    ZManaged.foreach(
        for {
          ontology <- ontologyOpt
          factory <- factoryOpt
        } yield ZIO
          .effect(factory.createReasoner(ontology))
          .mapError(e => DOSDPError(s"Failed to create reasoner for ontology $ontology", e))
          .toManaged(o => ZIO.effectTotal(o.dispose()))
    )(identity)

  private def createQuery(target: QueryTarget, config: QueryConfig, reasonerOpt: Option[OWLReasoner]): ZIO[Any, DOSDPError, String] =
    for {
      dosdp <- Config.inputDOSDPFrom(target.templateFile)
      prefixes <- config.common.prefixesMap
    } yield makeProcessedQuery(dosdp, prefixes, config.restrictAxiomsTo, reasonerOpt)

  def makeProcessedQuery(dosdp: DOSDP, prefixes: PartialFunction[String, String], axiomKind: AxiomKind, reasonerOpt: Option[OWLReasoner]): String = {
    val sparqlQuery = SPARQL.queryFor(ExpandedDOSDP(dosdp, prefixes), axiomKind)
    reasonerOpt
      .map { reasoner =>
        new Owlet(reasoner).expandQueryString(sparqlQuery)
      }
      .getOrElse(sparqlQuery)
  }

  private def processTarget(target: QueryTarget,
                            config: QueryConfig,
                            processedQuery: String,
                            model: Option[Model]): ZIO[Any, DOSDPError, Unit] = {
    val doPrintQuery = ZIO
      .effect(new PrintWriter(new File(target.outputFile), "utf-8"))
      .bracketAuto(w => ZIO.effect(w.print(processedQuery)))
    val doPerformQuery = for {
      (columns, results) <- performQuery(processedQuery, model)
      sepFormat <- ZIO.fromEither(Config.tabularFormat(config.common.tableFormat))
      _ <-
        ZIO
          .effect(CSVWriter.open(target.outputFile, "utf-8")(sepFormat))
          .bracketAuto(w => writeQueryResults(w, columns, results))
    } yield ()
    (if (config.printQuery.bool) doPrintQuery else doPerformQuery).mapError(e => DOSDPError("Failure performing query command", e))
  }

  private def writeQueryResults(writer: CSVWriter, columns: List[String], results: List[QuerySolution]) =
    ZIO.effect(writer.writeRow(columns)) *> ZIO.foreach(results) { qs =>
      ZIO.effect(writer.writeRow(columns.map(variable => Option(qs.get(variable)).map(_.toString).getOrElse(""))))
    }

  private def determineTargets(config: QueryConfig): RIO[Blocking, List[QueryTarget]] = {
    val patternNames = config.common.batchPatterns.items
    if (patternNames.nonEmpty) for {
      _ <- ZIO.effectTotal(scribe.info("Running in batch mode"))
      _ <- ZIO.ifM(isDirectory(config.common.template))(ZIO.unit, ZIO.fail(DOSDPError("\"--template must be a directory in batch mode\"")))
      _ <- ZIO.ifM(isDirectory(config.common.outfile))(ZIO.unit, ZIO.fail(DOSDPError("\"--outfile must be a directory in batch mode\"")))
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

  def performQuery(sparql: String, modelOpt: Option[Model]): Task[(List[String], List[QuerySolution])] =
    for {
      model <- ZIO.fromOption(modelOpt).orElseFail(DOSDPError("Can't use None Model"))
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

  final private case class QueryTarget(templateFile: String, outputFile: String)

}
