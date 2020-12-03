package org.monarchinitiative.dosdp

import org.apache.jena.query.QueryFactory
import org.apache.jena.sys.JenaSystem
import org.monarchinitiative.dosdp.cli.Config
import zio._
import zio.test.Assertion._
import zio.test._

import scala.jdk.CollectionConverters._

object QueryGeneratorTest extends DefaultRunnableSpec {

  JenaSystem.init()

  private val prefixes = Map(
    "BFO" -> "http://purl.obolibrary.org/obo/BFO_",
    "RO" -> "http://purl.obolibrary.org/obo/RO_"
  )

  def spec = suite("Test query generator") {
    testM("Defined class should be first column") {
      for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/QueryGeneratorTest.yaml")
        variables <- ZIO.effect(QueryFactory.create(SPARQL.queryFor(ExpandedDOSDP(dosdp, prefixes), Config.LogicalAxioms)).getProjectVars.asScala)
      } yield assert(variables(0).getVarName)(equalTo("defined_class")) &&
        assert(variables(1).getVarName)(equalTo("defined_class_label"))
    }
  }

}
