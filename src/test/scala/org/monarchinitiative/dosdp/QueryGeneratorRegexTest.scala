package org.monarchinitiative.dosdp

import org.apache.jena.sys.JenaSystem
import org.monarchinitiative.dosdp.cli.Config
import zio._
import zio.test.Assertion._
import zio.test._
import zio.logging._

object QueryGeneratorRegexTest extends DefaultRunnableSpec {

  JenaSystem.init()

  def spec = suite("Test query label token regex") {
    testM("Query label token regex") {
      for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/QueryGeneratorRegexTest.yaml")
        results <- SPARQL.triplesFor(ExpandedDOSDP(dosdp, OBOPrefixes), Config.AnnotationAxioms)
      } yield assert(results.forall(a => a.contains("vector-borne")))(isFalse) && assert(results.forall(a => !a.contains("infectious_disease123asdf")))(isFalse)
    }
  }.provideCustomLayer(Logging.consoleErr())

}
