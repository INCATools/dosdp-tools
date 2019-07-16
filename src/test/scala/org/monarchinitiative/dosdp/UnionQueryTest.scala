package org.monarchinitiative.dosdp

import java.io.File

import org.monarchinitiative.dosdp.cli.Query
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI

import scala.collection.JavaConverters._

class UnionQueryTest extends UnitSpec {

  val term = Class("http://purl.obolibrary.org/obo/ONT_0000001")
  val item = Class("http://purl.obolibrary.org/obo/ONT_0000002")
  val partOf = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000050")

  val dosdp = DOSDP.empty.copy(
    pattern_name = Some("test_unions_pattern"),
    classes = Some(Map(
      "thing" -> "owl:Thing",
      "classA" -> "http://example.org#A",
      "classB" -> "http://example.org#B"
    )),
    vars = Some(Map("item" -> "'thing'")),
    equivalentTo = Some(PrintfOWLConvenience(None, "'classA' or 'classB' or %s", Some(List("item")))))
  val sparqlQuery = SPARQL.queryFor(ExpandedDOSDP(dosdp, OBOPrefixes))
  val ontology = OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File("src/test/resources/org/monarchinitiative/dosdp/test_union.ofn")))

  "Unions" should "be queryable" in {
    val results = Query.performQuery(sparqlQuery, ontology).asScala.toList
    results.foreach { qs =>
      qs.getResource("defined_class").getURI shouldEqual "http://example.org#X"
      qs.getResource("item").getURI shouldEqual "http://example.org#C"
    }
  }


}
