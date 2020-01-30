package org.monarchinitiative.dosdp.cli

import java.io.{File, FileWriter, PrintWriter}

import scala.collection.JavaConverters._
import org.apache.jena.query.{QueryExecutionFactory, QueryFactory, QuerySolution, ResultSet}
import org.apache.jena.rdf.model.ModelFactory
import org.backuity.clist._
import org.monarchinitiative.dosdp._
import org.phenoscape.owlet.Owlet
import org.semanticweb.HermiT.ReasonerFactory
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntology
import com.github.tototoshi.csv.CSVWriter
import uk.ac.manchester.cs.jfact.JFactFactory

import scala.collection.JavaConverters._

object Query extends Command(description = "query an ontology for terms matching a Dead Simple OWL Design Pattern") with Common {

  var reasonerNameOpt = opt[Option[String]](name = "reasoner", description = "Reasoner to use for expanding variable constraints. Valid options are ELK, HermiT, or JFact.")
  var printQuery = opt[Boolean](name = "print-query", default = false, description = "Print generated query without running against ontology")

  def run(): Unit = {
    val sepFormat = tabularFormat
    val patternNames = batchPatterns
    val targets = if (patternNames.nonEmpty) {
      logger.info("Running in batch mode")
      if (!(new File(templateFile).isDirectory)) throw new UnsupportedOperationException(s"--template must be a directory in batch mode")
      if (!(outfile.isDirectory)) throw new UnsupportedOperationException(s"--outfile must be a directory in batch mode")
      patternNames.map { pattern =>
        val templateFileName = s"$templateFile/$pattern.yaml"
        val outFileName = s"$outfile/$pattern.rq"
        QueryTarget(templateFileName, outFileName)
      }
    } else List(QueryTarget(templateFile, outfile.toString))
    val reasonerFactoryOpt = reasonerNameOpt.map(_.toLowerCase).map {
      case "elk"    => new ElkReasonerFactory()
      case "hermit" => new ReasonerFactory()
      case "jfact"  => new JFactFactory()
      case other    => throw new RuntimeException(s"Reasoner $other not supported. Options are ELK, HermiT, or JFact")
    }
    val reasonerOpt = for {
      ontology <- ontologyOpt
      factory <- reasonerFactoryOpt
    } yield factory.createReasoner(ontology)
    targets.foreach { target =>
      val dosdp = inputDOSDPFrom(target.templateFile)
      val sparqlQuery = SPARQL.queryFor(ExpandedDOSDP(dosdp, prefixes))
      val processedQuery = reasonerOpt match {
        case Some(reasoner) =>
          val owlet = new Owlet(reasoner)
          owlet.expandQueryString(sparqlQuery)
        case None           => sparqlQuery
      }
      if (printQuery) {
        val writer = new PrintWriter(new File(target.outputFile), "utf-8")
        writer.print(processedQuery)
        writer.close()
      } else {
        val ont = ontologyOpt.getOrElse(throw new RuntimeException("Can't run query; no ontology provided."))
        val results = performQuery(processedQuery, ont)
        val columns = results.getResultVars.asScala.toList
        val writer = CSVWriter.open(target.outputFile, "utf-8")(sepFormat)
        writer.writeRow(columns)
        while (results.hasNext) {
          val qs = results.next()
          writer.writeRow(columns.map(variable => Option(qs.get(variable)).map(_.toString).getOrElse("")))
        }
        writer.close()
      }
    }

  }

  def performQuery(sparql: String, ont: OWLOntology): ResultSet = {
    val model = ModelFactory.createDefaultModel()
    val allAxioms = for {
      completeOnt <- ont.getImportsClosure.asScala
      axiom <- completeOnt.getAxioms().asScala
    } yield axiom
    val manager = OWLManager.createOWLOntologyManager()
    val triples = SesameJena.ontologyAsTriples(manager.createOntology(allAxioms.asJava))
    model.add(triples.toList.asJava)
    val query = QueryFactory.create(sparql)
    QueryExecutionFactory.create(query, model).execSelect()
  }

  private final case class QueryTarget(templateFile: String, outputFile: String)

}