package org.monarchinitiative.dosdp

import org.apache.jena.query.QuerySolution
import org.apache.jena.sys.JenaSystem
import org.monarchinitiative.dosdp.cli.{Config, Query}
import zio.ZIO
import zio.test.Assertion._
import zio.test._

object QueryLabelTest extends DefaultRunnableSpec {

  JenaSystem.init()

  def spec = suite("Test query label components") {
    testM("Query label components") {
      for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/QueryLabelTest.yaml")
        ontology <- Utilities.loadOntology("src/test/resources/org/monarchinitiative/dosdp/QueryLabelTest.ofn", None)
        model <- Query.makeModel(ontology)
        query <- ZIO.fromEither(Query.makeProcessedQuery(dosdp, OBOPrefixes, Config.AnnotationAxioms, None))
        (_, results) <- Query.performQuery(query, model)
      } yield {
        // Should match on any readable identifier (either label or synonym)
        assert(results.exists(containsResourceBindings(_, Map(
          "defined_class" -> "http://example.org/A",
          "entity" -> "http://example.org/B",
          "attribute" -> "http://example.org/C"
        ))))(isTrue) &&
          // Should match regardless of plain literal or xsd:string
          assert(results.exists(containsResourceBindings(_, Map(
            "defined_class" -> "http://example.org/G",
            "entity" -> "http://example.org/I",
            "attribute" -> "http://example.org/H"
          ))))(isTrue)
      }
    }
  }

  private def containsResourceBindings(qs: QuerySolution, bindings: Map[String, String]): Boolean =
    bindings.forall { case (variable, value) =>
      qs.getResource(variable).getURI == value
    }

}
