package org.monarchinitiative.dosdp.cli

import java.io.{File, PrintWriter}

import com.github.tototoshi.csv.{CSVWriter, TSVFormat}
import org.apache.jena.query.{QueryExecutionFactory, QueryFactory, QuerySolution}
import org.apache.jena.rdf.model.ModelFactory
import org.monarchinitiative.dosdp.Utilities.isDirectory
import org.monarchinitiative.dosdp.{ExpandedDOSDP, SPARQL, SesameJena}
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

object Query extends Logging {

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
      _ <- makeOptionalReasoner(ontologyOpt, reasonerFactoryOpt).use { reasonerOpt =>
        ZIO.foreach(targets) { target =>
          makeProcessedQuery(target, config, reasonerOpt).flatMap(processTarget(target, config, _, ontologyOpt))
        }
      }
    } yield ()
  }

  private def makeOptionalReasoner(ontologyOpt: Option[OWLOntology], factoryOpt: Option[OWLReasonerFactory]): ZManaged[Any, DOSDPError, Option[OWLReasoner]] = {
    ZManaged.foreach(for {
      ontology <- ontologyOpt
      factory <- factoryOpt
    } yield ZIO.effect(factory.createReasoner(ontology))
      .mapError(e => DOSDPError(s"Failed to create reasoner for ontology $ontology", e))
      .toManaged(o => ZIO.effectTotal(o.dispose())))(identity)
  }

  private def makeProcessedQuery(target: QueryTarget, config: QueryConfig, reasonerOpt: Option[OWLReasoner]): ZIO[Any, DOSDPError, String] = {
    for {
      _ <- logInfo(s"Processing pattern ${target.templateFile}")
      dosdp <- Config.inputDOSDPFrom(target.templateFile)
      prefixes <- config.common.prefixesMap
      sparqlQuery = SPARQL.queryFor(ExpandedDOSDP(dosdp, prefixes), config.restrictAxiomsTo)
      processedQuery = reasonerOpt.map { reasoner =>
        new Owlet(reasoner).expandQueryString(sparqlQuery)
      }.getOrElse(sparqlQuery)
    } yield processedQuery
  }

  private def processTarget(target: QueryTarget, config: QueryConfig, processedQuery: String, ontologyOpt: Option[OWLOntology]): ZIO[Any, DOSDPError, Unit] = {
    val doPrintQuery = ZIO.effect(new PrintWriter(new File(target.outputFile), "utf-8"))
      .bracketAuto(w => ZIO.effect(w.print(processedQuery)))
    val doPerformQuery = for {
      ont <- ZIO.fromOption(ontologyOpt).orElseFail(DOSDPError("Can't run query; no ontology provided."))
      (columns, results) <- performQuery(processedQuery, ont)
      sepFormat <- ZIO.fromEither(Config.tabularFormat(config.common.tableFormat))
      _ <- ZIO.effect(CSVWriter.open(target.outputFile, "utf-8")(sepFormat))
        .bracketAuto(w => writeQueryResults(w, columns, results))
    } yield ()
    logInfo(s"Processing pattern ${target.templateFile}") *>
      (if (config.printQuery.bool) doPrintQuery else doPerformQuery).mapError(e => DOSDPError("Failure performing query command", e))
  }

  private def writeQueryResults(writer: CSVWriter, columns: List[String], results: List[QuerySolution]) =
    ZIO.effect(writer.writeRow(columns)) *> ZIO.foreach(results) { qs =>
      ZIO.effect(writer.writeRow(columns.map(variable => Option(qs.get(variable)).map(_.toString).getOrElse(""))))
    }

  private def determineTargets(config: QueryConfig): RIO[Blocking, List[QueryTarget]] = {
    val sepFormat = Config.tabularFormat(config.common.tableFormat)
    val patternNames = config.common.batchPatterns.items
    if (patternNames.nonEmpty) for {
      _ <- logInfo("Running in batch mode")
      _ <- ZIO.ifM(isDirectory(config.common.template))(ZIO.unit,
        ZIO.fail(DOSDPError("\"--template must be a directory in batch mode\"")))
      _ <- ZIO.ifM(isDirectory(config.common.outfile))(ZIO.unit,
        ZIO.fail(DOSDPError("\"--outfile must be a directory in batch mode\"")))
    } yield patternNames.map { pattern =>
      val templateFileName = s"${config.common.template}/$pattern.yaml"
      val suffix = if (config.printQuery.bool) "rq"
      else config.common.tableFormat.toLowerCase
      val outFileName = s"${config.common.outfile}/$pattern.$suffix"
      QueryTarget(templateFileName, outFileName)
    }
    else ZIO.succeed(List(QueryTarget(config.common.template, config.common.outfile)))
  }

  def performQuery(sparql: String, ont: OWLOntology): Task[(List[String], List[QuerySolution])] = {
    for {
      model <- ZIO.effectTotal(ModelFactory.createDefaultModel())
      allAxioms = for {
        completeOnt <- ont.getImportsClosure.asScala.to(Set)
        axiom <- completeOnt.getAxioms().asScala.to(Set)
      } yield axiom
      manager <- ZIO.effectTotal(OWLManager.createOWLOntologyManager())
      ont <- ZIO.effect(manager.createOntology(allAxioms.asJava))
      triples = SesameJena.ontologyAsTriples(ont)
      _ <- ZIO.effect(model.add(triples.toList.asJava))
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
  }

  private final case class QueryTarget(templateFile: String, outputFile: String)

}
