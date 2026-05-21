package org.monarchinitiative.dosdp

import org.apache.jena.sys.JenaSystem
import org.monarchinitiative.dosdp.cli.Config
import zio.{Config => _, _}
import zio.test.Assertion._
import zio.test._

object QueryGeneratorRegexTest extends ZIOSpecDefault {

  JenaSystem.init()

  def spec = suite("Test query label token regex")(
    test("Query label token regex") {
      for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/QueryGeneratorRegexTest.yaml")
        compiled <- PatternCompiler.compile(dosdp, OBOPrefixes)
        results <- SPARQL.triplesFor(compiled, Config.AnnotationAxioms)
        joined = results.mkString("\n")
      } yield assert(results.forall(a => a.contains("vector-borne")))(isFalse) &&
        assert(results.forall(a => !a.contains("infectious_disease123asdf")))(isFalse) &&
        assert(joined)(containsString("?vector ?vector__match_property")) &&
        assert(joined)(containsString("?infectious_disease ?infectious_disease__match_property")) &&
        assert(joined)(not(containsString("vector_borne"))) &&
        assert(joined)(not(containsString("vector_x002d_borne")))
    }
  )

}
