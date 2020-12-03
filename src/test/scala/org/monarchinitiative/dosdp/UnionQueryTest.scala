package org.monarchinitiative.dosdp

import org.apache.jena.sys.JenaSystem
import org.monarchinitiative.dosdp.cli.{Config, Query}
import org.phenoscape.scowl.{not => _, _}
import org.semanticweb.owlapi.model.{OWLClass, OWLObjectProperty}
import zio._
import zio.test.Assertion._
import zio.test._


object UnionQueryTest extends DefaultRunnableSpec {

  JenaSystem.init()

  val term: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000001")
  val item: OWLClass = Class("http://purl.obolibrary.org/obo/ONT_0000002")
  val partOf: OWLObjectProperty = ObjectProperty("http://purl.obolibrary.org/obo/BFO_0000050")

  val dosdp: DOSDP = DOSDP.empty.copy(
    pattern_name = Some("test_unions_pattern"),
    classes = Some(Map(
      "thing" -> "owl:Thing",
      "classA" -> "http://example.org#A",
      "classB" -> "http://example.org#B"
    )),
    vars = Some(Map("item" -> "'thing'")),
    equivalentTo = Some(PrintfOWLConvenience(None, "'classA' or 'classB' or %s", Some(List("item")))))
  val sparqlQuery: String = SPARQL.queryFor(ExpandedDOSDP(dosdp, OBOPrefixes), Config.LogicalAxioms)

  def spec = suite("Union query test") {
    testM("Unions should be queryable") {
      for {
        ontology <- Utilities.loadOntology("src/test/resources/org/monarchinitiative/dosdp/test_union.ofn", None)
        model <- Query.makeModel(Some(ontology))
        columnsAndResults <- Query.performQuery(sparqlQuery, model)
        (_, results) = columnsAndResults
        tests <- ZIO.foreach(results) { qs =>
          for {
            definedClass <- ZIO.effect(qs.getResource("defined_class").getURI)
            item <- ZIO.effect(qs.getResource("item").getURI)
          } yield assert(definedClass)(equalTo("http://example.org#X")) &&
            assert(item)(equalTo("http://example.org#C"))
        }
      } yield tests.reduce(_ && _)
    }
  }

}
