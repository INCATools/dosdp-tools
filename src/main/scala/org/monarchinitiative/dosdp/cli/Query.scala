package org.monarchinitiative.dosdp.cli

import java.io.File
import java.io.FileReader

import org.backuity.clist._
import org.phenoscape.owlet.Owlet
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.yaml.parser
import org.apache.jena.riot.RDFDataMgr

import java.io.FileOutputStream
import org.apache.jena.query.QueryFactory
import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.query.ResultSetFormatter

import scala.collection.JavaConverters._
import org.semanticweb.owlapi.model.OWLOntology
import org.apache.jena.rdf.model.ModelFactory
import org.monarchinitiative.dosdp._

object Query extends Command(description = "query an ontology for terms matching a Dead Simple OWL Design Pattern") with Common {

  var reasonerNameOpt = opt[Option[String]](name = "reasoner", description = "Reasoner to use for expanding variable constraints (currently only valid option is `elk`)")
  var printQuery = opt[Boolean](name = "print-query", default = false, description = "Print generated query without running against ontology")

  def run: Unit = {
    for {
      dosdp <- inputDOSDP.right
    } {
      val sparqlQuery = SPARQL.queryFor(ExpandedDOSDP(dosdp, prefixes))
      val processedQuery = (ontologyOpt, reasonerNameOpt) match {
        case (None, Some(_)) => throw new RuntimeException("Reasoner requested but no ontology specified; exiting.")
        case (Some(ontology), Some("elk")) => {
          val reasoner = new ElkReasonerFactory().createReasoner(ontology)
          val owlet = new Owlet(reasoner)
          owlet.expandQueryString(sparqlQuery)
        }
        case (Some(ontology), Some(otherReasoner)) => throw new RuntimeException(s"$otherReasoner not supported as reasoner.")
        case (_, None)                             => sparqlQuery
      }
      if (printQuery) {
        println(processedQuery)
      } else {
        if (ontOpt.isEmpty) throw new RuntimeException("Can't run query; no ontology provided.")
        val triples = for {
          mainOnt <- ontologyOpt.toSet[OWLOntology]
          ont <- mainOnt.getImportsClosure.asScala
          triple <- SesameJena.ontologyAsTriples(ont)
        } yield triple
        val model = ModelFactory.createDefaultModel()
        model.add(triples.toList.asJava)
        val query = QueryFactory.create(processedQuery)
        val results = QueryExecutionFactory.create(query, model).execSelect()
        ResultSetFormatter.outputAsTSV(new FileOutputStream(outfile), results)
      }
    }
  }

}