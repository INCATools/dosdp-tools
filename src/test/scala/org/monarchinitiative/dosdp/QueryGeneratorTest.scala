package org.monarchinitiative.dosdp

import org.apache.jena.query.QueryFactory
import org.apache.jena.sys.JenaSystem
import org.monarchinitiative.dosdp.cli.Config
import zio.{Config => _, _}
import zio.test.Assertion._
import zio.test._

import scala.jdk.CollectionConverters._

object QueryGeneratorTest extends ZIOSpecDefault {

  JenaSystem.init()

  private val prefixes = Map(
    "BFO" -> "http://purl.obolibrary.org/obo/BFO_",
    "RO" -> "http://purl.obolibrary.org/obo/RO_"
  )

  def spec = suite("Test query generator") {
    test("Defined class should be first column") {
      for {
        dosdp <- Config.inputDOSDPFrom("src/test/resources/org/monarchinitiative/dosdp/QueryGeneratorTest.yaml")
        compiled <- PatternCompiler.compile(dosdp, prefixes)
        query <- SPARQL.queryFor(compiled, Config.LogicalAxioms)
        variables <- ZIO.attempt(QueryFactory.create(query).getProjectVars.asScala)
      } yield assert(variables(0).getVarName)(equalTo("defined_class")) &&
        assert(variables(1).getVarName)(equalTo("defined_class_label"))
    }
  }

}
