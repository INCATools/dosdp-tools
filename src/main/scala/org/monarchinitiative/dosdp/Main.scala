package org.monarchinitiative.dosdp

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
import io.circe.yaml.parser.Parser
import org.apache.jena.riot.RDFDataMgr
import com.hp.hpl.jena.query.QueryFactory
import com.hp.hpl.jena.query.QueryExecutionFactory
import com.hp.hpl.jena.query.ResultSetFormatter

object Main extends CliMain[Unit](
  name = "dosdp-query",
  description = "query an ontology for terms matching a Dead Simple OWL Design Pattern") {

  var ontOpt = opt[Option[String]](name = "ontology", description = "OWL ontology to query")
  var templateFile = opt[File](name = "template", default = new File("dosdp.yaml"), description = "DOSDP file (YAML)")
  var reasonerNameOpt = opt[Option[String]](name = "reasoner", description = "Reasoner to use for expanding variable constraints (currently any reasoner as long as it is `elk`")
  var printQuery = opt[Boolean](name = "print-query", default = false, description = "Print generated query without running against ontology")

  def run: Unit = {
    val ontIRIOpt = ontOpt.map(ontPath => if (ontPath.startsWith("http")) IRI.create(ontPath) else IRI.create(new File(ontPath)))
    val ontologyOpt = ontIRIOpt.map { ontIRI =>
      val manager = OWLManager.createOWLOntologyManager()
      manager.loadOntology(ontIRI)
    }
    for {
      json <- Parser.parse(new FileReader(templateFile))
      dosdp <- decode[DOSDP](json.spaces4)
    } yield {
      val sparqlQuery = SPARQL.queryFor(dosdp)
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
        //TODO Currently this is loading (or downloading) the ontology again. Maybe it can load from the OWL
        val model = ontOpt.map(ontPath => RDFDataMgr.loadModel(ontPath)).getOrElse(throw new RuntimeException("Can't run query; no ontology provided."))
        val query = QueryFactory.create(processedQuery)
        val results = QueryExecutionFactory.create(query, model).execSelect()
        ResultSetFormatter.outputAsTSV(System.out, results)
      }
    }
  }

}